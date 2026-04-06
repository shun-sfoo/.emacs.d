---
id: "2010992883457615820"
title: "用 C++ 写线程池是怎样一种体验？"
author: "花宝宝"
type: zhihu-answer
source: "https://www.zhihu.com/question/27908489/answer/2010992883457615820"
downloaded: "2026-03-29"
---
写完第一版你会觉得自己很牛。写完第二版你会觉得 CPU 很坑。写完第三版你会觉得 Go 真香。

## **第一版：50 行，信心爆棚**

[线程池](https://zhida.zhihu.com/search?content_id=771044108&content_type=Answer&match_order=1&q=%E7%BA%BF%E7%A8%8B%E6%B1%A0&zhida_source=entity)的核心概念其实很简单——一个[任务队列](https://zhida.zhihu.com/search?content_id=771044108&content_type=Answer&match_order=1&q=%E4%BB%BB%E5%8A%A1%E9%98%9F%E5%88%97&zhida_source=entity)、一组[工作线程](https://zhida.zhihu.com/search?content_id=771044108&content_type=Answer&match_order=1&q=%E5%B7%A5%E4%BD%9C%E7%BA%BF%E7%A8%8B&zhida_source=entity)、一把锁加一个[条件变量](https://zhida.zhihu.com/search?content_id=771044108&content_type=Answer&match_order=1&q=%E6%9D%A1%E4%BB%B6%E5%8F%98%E9%87%8F&zhida_source=entity)。工作线程循环从队列里取任务执行，没任务就等着。

```cpp
class ThreadPool {
public:
    explicit ThreadPool(size_t threads) : stop_(false) {
        for (size_t i = 0; i < threads; ++i) {
            workers_.emplace_back([this] {
                while (true) {
                    std::function<void()> task;
                    {
                        std::unique_lock<std::mutex> lock(mutex_);
                        cv_.wait(lock, [this] { 
                            return stop_ || !tasks_.empty(); 
                        });
                        if (stop_ && tasks_.empty()) return;
                        task = std::move(tasks_.front());
                        tasks_.pop();
                    }
                    task();
                }
            });
        }
    }

    template<class F, class... Args>
    auto submit(F&& f, Args&&... args) 
        -> std::future<std::invoke_result_t<F, Args...>> 
    {
        using return_type = std::invoke_result_t<F, Args...>;
        auto task = std::make_shared<std::packaged_task<return_type()>>(
            std::bind(std::forward<F>(f), std::forward<Args>(args)...)
        );
        std::future<return_type> result = task->get_future();
        {
            std::lock_guard<std::mutex> lock(mutex_);
            tasks_.emplace([task]() { (*task)(); });
        }
        cv_.notify_one();
        return result;
    }

    ~ThreadPool() {
        {
            std::lock_guard<std::mutex> lock(mutex_);
            stop_ = true;
        }
        cv_.notify_all();
        for (auto& w : workers_) w.join();
    }

private:
    std::vector<std::thread> workers_;
    std::queue<std::function<void()>> tasks_;
    std::mutex mutex_;
    std::condition_variable cv_;
    bool stop_;
};
```

用起来很舒服：

```cpp
ThreadPool pool(4);
auto future = pool.submit([](int a, int b) { return a + b; }, 3, 4);
std::cout << future.get() << std::endl;  // 7
```

50 行代码，支持任意参数、返回 future、能优雅关停。这时候你的感觉是"线程池不过如此"。

然后你开始压测。

## **线程开到 16 个，反而变慢了**

4 个线程的时候一切正常。线程数上到 16，吞吐量不升反降。用 `perf` 一看：

```bash
perf record -g ./your_program
perf report
```

大量时间花在 `__pthread_mutex_lock` 和 `futex` 上。工作线程不是在干活，是在排队等那一把 `mutex_`。

所有线程共享一个任务队列、一把锁，这就是个单车道收费站——车道修得再宽，收费口只有一个，车越多越堵。

解决思路是给每个线程一个本地队列，任务优先从自己的队列取。本地队列空了就去偷别人的——这就是 [work-stealing](https://zhida.zhihu.com/search?content_id=771044108&content_type=Answer&match_order=1&q=work-stealing&zhida_source=entity)，Java 的 `ForkJoinPool` 和 Go 的 GMP 调度器用的都是这招。

```cpp
std::function<void()> steal_task(size_t thief_id) {
    for (size_t i = 0; i < worker_data_.size(); ++i) {
        size_t victim = (thief_id + i + 1) % worker_data_.size();
        std::lock_guard<std::mutex> lock(worker_data_[victim].local_mutex);
        if (!worker_data_[victim].local_queue.empty()) {
            auto task = std::move(worker_data_[victim].local_queue.back());
            worker_data_[victim].local_queue.pop_back();
            return task;
        }
    }
    return nullptr;
}
```

偷的时候从队列**尾部**取，自己取任务从**头部**取。两端操作减少锁冲突，而且自己刚放进去的任务很可能还在 L1 cache 里。

好，改完之后性能确实上来了。你又开始觉得自己很牛了。

## **然后 [false sharing](https://zhida.zhihu.com/search?content_id=771044108&content_type=Answer&match_order=1&q=false+sharing&zhida_source=entity) 教你做人**

压测的时候你给每个线程加了个计数器统计完成了多少任务：

```cpp
struct WorkerData {
    std::atomic<uint64_t> tasks_completed{0};
    std::deque<std::function<void()>> local_queue;
    std::mutex local_mutex;
};
```

加完之后性能又崩了。一个简单的原子计数器怎么会影响性能？

因为两个相邻的 `WorkerData` 可能落在同一条 [CPU 缓存行](https://zhida.zhihu.com/search?content_id=771044108&content_type=Answer&match_order=1&q=CPU+%E7%BC%93%E5%AD%98%E8%A1%8C&zhida_source=entity)里（cache line，64 字节）。线程 A 改自己的 `tasks_completed`，CPU 的 MESI 缓存一致性协议会把线程 B 的那条缓存行也标记为失效——虽然 B 完全没碰 A 的数据。这就是 false sharing，两个不相关的变量因为物理位置太近而互相拖累。

Intel 的工程师在 "Avoiding and Identifying False Sharing Among Threads" 里做过测试，一个简单的计数器场景，false sharing 导致性能下降超过 10 倍。

修复方法就一行：

```cpp
struct alignas(64) WorkerData {
    std::atomic<uint64_t> tasks_completed{0};
    std::deque<std::function<void()>> local_queue;
    std::mutex local_mutex;
};
```

`alignas(64)` 保证每个 `WorkerData` 独占自己的缓存行。C++17 定义了 `std::hardware_destructive_interference_size` 本来该干这事，GCC 和 MSVC 实现了，但 Clang 一直拒绝实现（理由是缓存行大小是运行时属性，编译时常量不靠谱），直接写 64 是最稳的做法。

一行代码改完，性能翻倍。你的感觉从"我很牛"变成了"CPU 缓存这东西真的能在代码里看不到任何问题的情况下把性能搞崩"。

## **`std::function` 也在偷偷 new**

接着压测，发现高频提交小任务的时候性能不及预期。问题在 `std::function<void()>`。

`std::function` 内部有 small buffer optimization——小的 callable 放栈上，大的要堆分配。libstdc++ 的 SBO 缓冲区只有 16 字节，一个捕获了三四个变量的 lambda 轻松超过这个限制。每个任务一次 `new` 一次 `delete`，频率上去之后堆分配就变成热点了。

自己撸一个固定大小的栈上 callable：

```cpp
template<size_t BufferSize = 64>
class InplaceFunction {
    alignas(std::max_align_t) char buffer_[BufferSize];
    void (*invoke_)(void*);
    void (*destroy_)(void*);
    
public:
    template<class F>
    InplaceFunction(F&& f) {
        static_assert(sizeof(F) <= BufferSize, "callable too large");
        new (buffer_) F(std::forward<F>(f));
        invoke_ = [](void* p) { (*static_cast<F*>(p))(); };
        destroy_ = [](void* p) { static_cast<F*>(p)->~F(); };
    }
    
    void operator()() { invoke_(buffer_); }
    ~InplaceFunction() { destroy_(buffer_); }
};
```

64 字节缓冲区，够放绝大多数 lambda。超了直接 `static_assert` 编译报错。

到这一步你已经不觉得自己在写线程池了。你在跟编译器的内存布局、CPU 的缓存行、glibc 的 malloc 实现斗智斗勇。最初那个 50 行的"核心逻辑"早就淹没在这些基础设施的优化里了。

## **Go 程序员全程看戏**

Go 写同样的东西：

```go
for i := 0; i < 100; i++ {
    go func(n int) {
        fmt.Println(n * n)
    }(i)
}
```

goroutine 创建成本大约 2KB 栈空间，会自动增长。GMP 调度器内置 work-stealing。缓存行、堆分配这些问题 runtime 全给你兜了。

但 Go 方案也有它的代价。goroutine 调度器在用户态跑，每次函数调用都可能触发调度检查（Go 1.14 之后加了基于信号的抢占）。对于微秒级延迟敏感的场景——量化交易、音视频实时处理——GMP 调度器引入的不确定性是不能接受的。

C++ 手写线程池可以用 `pthread_setaffinity_np` 把线程钉死在特定 CPU 核上，避免跨核调度导致的缓存失效：

```cpp
cpu_set_t cpuset;
CPU_ZERO(&cpuset);
CPU_SET(core_id, &cpuset);
pthread_setaffinity_np(pthread_self(), sizeof(cpu_set_t), &cpuset);
```

方便和可控，两头不可兼得。大部分场景 Go 的方便更值钱，少数场景 C++ 的可控是刚需。

## **C++23 之后**

`std::jthread` 自带协作式取消，不用手动管线程生命周期了。[P2300](https://zhida.zhihu.com/search?content_id=771044108&content_type=Answer&match_order=1&q=P2300&zhida_source=entity) 提案（`std::execution`）的 sender/receiver 模型预计在 C++26 进入标准，提供标准的异步执行框架。各编译器的实验性支持已经在推进，方向很清楚——以后大部分场景不需要自己从零写线程池了。

不过话说回来，用 C++ 手写一次线程池的体验本身就挺有价值。不是代码的价值，是你会真切感受到"一把锁能让 16 个线程变成单线程"、"两个变量挨得太近就能让性能掉十倍"这些在教科书里只是一句话的东西。

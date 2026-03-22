CXX = c++
CXXFLAGS = -O3 -std=c++23 -pthread
LDFLAGS = -lm

plasma_mt: plasma_mt.cpp
	$(CXX) $(CXXFLAGS) -o plasma_mt plasma_mt.cpp $(LDFLAGS)

clean:
	rm -f plasma_mt output-*.ppm output.mp4

run:
	./plasma_mt

video:
	ffmpeg -i output-%03d.ppm -r 60 output.mp4

show:
	mpv output.mp4 

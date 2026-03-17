use emacs::{defun, Env, Result, Value};
use libmpv::Mpv;
use once_cell::sync::Lazy;
use std::sync::Mutex;

emacs::plugin_is_GPL_compatible!();

static MPV: Lazy<Mutex<Option<Mpv>>> = Lazy::new(|| Mutex::new(None));
static CURRENT_TIME: Lazy<Mutex<f64>> = Lazy::new(|| Mutex::new(-1.0));
static LISTENER_STARTED: Lazy<Mutex<bool>> = Lazy::new(|| Mutex::new(false));

#[emacs::module(name = "mpv")]
fn init(env: &Env) -> Result<Value<'_>> {
    let mut mpv_guard = MPV.lock().unwrap();
    match Mpv::new() {
        Ok(mpv) => {
            *mpv_guard = Some(mpv);
            env.message("mpv plugin loaded successfully")
        }
        Err(e) => {
            let msg = format!("Failed to create mpv instance: {}", e);
            env.message(&msg)?;
            anyhow::bail!(msg)
        }
    }
}

#[defun]
fn play(env: &Env, path: String) -> Result<Value<'_>> {
    let mpv_guard = MPV.lock().unwrap();
    let mpv = match mpv_guard.as_ref() {
        Some(m) => m,
        None => {
            anyhow::bail!("mpv not initialized")
        }
    };
    if let Err(e) = mpv.command("loadfile", &[&path]) {
        anyhow::bail!(format!("Failed to play: {}", e));
    }
    env.message(&format!("Playing: {}", path))
}

#[defun]
fn stop(env: &Env) -> Result<Value<'_>> {
    let mpv_guard = MPV.lock().unwrap();
    let mpv = match mpv_guard.as_ref() {
        Some(m) => m,
        None => {
            anyhow::bail!("mpv not initialized")
        }
    };
    if let Err(e) = mpv.command("stop", &[]) {
        anyhow::bail!(format!("Failed to stop: {}", e));
    }
    env.message("Stopped")
}

#[defun]
fn pause(env: &Env) -> Result<Value<'_>> {
    let mpv_guard = MPV.lock().unwrap();
    let mpv = match mpv_guard.as_ref() {
        Some(m) => m,
        None => {
            anyhow::bail!("mpv not initialized")
        }
    };
    if let Err(e) = mpv.pause() {
        anyhow::bail!(format!("Failed to pause: {}", e));
    }
    env.message("Paused")
}

#[defun]
fn resume(env: &Env) -> Result<Value<'_>> {
    let mpv_guard = MPV.lock().unwrap();
    let mpv = match mpv_guard.as_ref() {
        Some(m) => m,
        None => {
            anyhow::bail!("mpv not initialized")
        }
    };
    if let Err(e) = mpv.unpause() {
        anyhow::bail!(format!("Failed to resume: {}", e));
    }
    env.message("Resumed")
}

#[defun]
fn get_time(env: &Env) -> Result<f64> {
    start_time_listener();

    let mpv_guard = MPV.lock().unwrap();
    let mpv = match mpv_guard.as_ref() {
        Some(m) => m,
        None => {
            anyhow::bail!("mpv not initialized")
        }
    };
    match mpv.get_property::<f64>("time-pos") {
        Ok(time) => {
            let mut current = CURRENT_TIME.lock().unwrap();
            *current = time;
            Ok(time)
        }
        Err(e) => Err(anyhow::anyhow!("Failed to get time: {}", e)),
    }
}

fn start_time_listener() {
    let mut started = LISTENER_STARTED.lock().unwrap();
    if *started {
        return;
    }
    *started = true;
    drop(started);

    std::thread::spawn(|| loop {
        std::thread::sleep(std::time::Duration::from_millis(50));
        if let Ok(mpv_guard) = MPV.lock() {
            if let Some(mpv) = mpv_guard.as_ref() {
                if let Ok(time) = mpv.get_property::<f64>("time-pos") {
                    if let Ok(mut current) = CURRENT_TIME.lock() {
                        if (time - *current).abs() > 0.01 {
                            *current = time;
                        }
                    }
                }
            }
        }
    });
}

#[defun]
fn load_subtitle(env: &Env, path: String) -> Result<Value<'_>> {
    let mpv_guard = MPV.lock().unwrap();
    let mpv = match mpv_guard.as_ref() {
        Some(m) => m,
        None => {
            anyhow::bail!("mpv not initialized")
        }
    };
    if let Err(e) = mpv.set_property("sub-file", path.as_str()) {
        anyhow::bail!(format!("Failed to load subtitle: {}", e));
    }
    env.message(&format!("Loaded subtitle: {}", path))
}

#[defun]
fn exit(env: &Env) -> Result<Value<'_>> {
    let mpv_guard = MPV.lock().unwrap();
    if let Some(mpv) = mpv_guard.as_ref() {
        if let Err(e) = mpv.command("quit", &[]) {
            anyhow::bail!(format!("Failed to exit mpv: {}", e));
        }
    }
    env.message("mpv quit command sent")
}

#[defun]
fn reinit(env: &Env) -> Result<Value<'_>> {
    let mut mpv_guard = MPV.lock().unwrap();
    match Mpv::new() {
        Ok(mpv) => {
            *mpv_guard = Some(mpv);
            env.message("mpv reinitialized")
        }
        Err(e) => {
            let msg = format!("Failed to reinitialize mpv: {}", e);
            anyhow::bail!(msg)
        }
    }
}

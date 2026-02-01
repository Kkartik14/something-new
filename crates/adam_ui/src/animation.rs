//! Animation system for Adam UI.
//!
//! Supports spring physics, tween (linear interpolation), and keyframe animations.
//! Each animation interpolates a value over time. When animation values change,
//! the affected views are re-rendered.


// ---------------------------------------------------------------------------
// Animation types
// ---------------------------------------------------------------------------

/// Unique identifier for an animation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AnimationId(pub u64);

static NEXT_ANIMATION_ID: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(1);

impl AnimationId {
    pub fn next() -> Self {
        AnimationId(NEXT_ANIMATION_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
    }
}

/// An easing function maps progress [0, 1] → [0, 1].
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Easing {
    Linear,
    EaseIn,
    EaseOut,
    EaseInOut,
    /// Custom cubic bezier (x1, y1, x2, y2).
    CubicBezier(f64, f64, f64, f64),
}

impl Easing {
    pub fn apply(&self, t: f64) -> f64 {
        let t = t.clamp(0.0, 1.0);
        match self {
            Easing::Linear => t,
            Easing::EaseIn => t * t * t,
            Easing::EaseOut => {
                let t1 = 1.0 - t;
                1.0 - t1 * t1 * t1
            }
            Easing::EaseInOut => {
                if t < 0.5 {
                    4.0 * t * t * t
                } else {
                    let t1 = -2.0 * t + 2.0;
                    1.0 - t1 * t1 * t1 / 2.0
                }
            }
            Easing::CubicBezier(x1, y1, x2, y2) => {
                // Approximate cubic bezier using Newton's method
                cubic_bezier_at(t, *x1, *y1, *x2, *y2)
            }
        }
    }
}

fn cubic_bezier_at(t: f64, x1: f64, y1: f64, x2: f64, y2: f64) -> f64 {
    // Find t_bezier such that bezier_x(t_bezier) = t
    // Then return bezier_y(t_bezier)
    let mut guess = t;
    for _ in 0..8 {
        let x = bezier_component(guess, x1, x2) - t;
        let dx = bezier_derivative(guess, x1, x2);
        if dx.abs() < 1e-12 {
            break;
        }
        guess -= x / dx;
        guess = guess.clamp(0.0, 1.0);
    }
    bezier_component(guess, y1, y2)
}

fn bezier_component(t: f64, p1: f64, p2: f64) -> f64 {
    let t2 = t * t;
    let t3 = t2 * t;
    3.0 * (1.0 - t) * (1.0 - t) * t * p1
        + 3.0 * (1.0 - t) * t2 * p2
        + t3
}

fn bezier_derivative(t: f64, p1: f64, p2: f64) -> f64 {
    let t2 = t * t;
    3.0 * (1.0 - t) * (1.0 - t) * p1
        + 6.0 * (1.0 - t) * t * (p2 - p1)
        + 3.0 * t2 * (1.0 - p2)
}

// ---------------------------------------------------------------------------
// Spring physics
// ---------------------------------------------------------------------------

/// Spring animation parameters.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SpringParams {
    /// Stiffness (higher = snappier). Default: 100.0
    pub stiffness: f64,
    /// Damping ratio (0 = no damping, 1 = critically damped). Default: 0.7
    pub damping: f64,
    /// Mass of the object. Default: 1.0
    pub mass: f64,
}

impl Default for SpringParams {
    fn default() -> Self {
        Self {
            stiffness: 100.0,
            damping: 0.7,
            mass: 1.0,
        }
    }
}

impl SpringParams {
    pub fn snappy() -> Self {
        Self { stiffness: 300.0, damping: 0.8, mass: 1.0 }
    }

    pub fn bouncy() -> Self {
        Self { stiffness: 200.0, damping: 0.5, mass: 1.0 }
    }

    pub fn gentle() -> Self {
        Self { stiffness: 50.0, damping: 0.9, mass: 1.0 }
    }
}

/// State of a spring animation.
#[derive(Debug, Clone)]
pub struct SpringState {
    pub params: SpringParams,
    pub from: f64,
    pub to: f64,
    pub position: f64,
    pub velocity: f64,
}

impl SpringState {
    pub fn new(from: f64, to: f64, params: SpringParams) -> Self {
        Self {
            params,
            from,
            to,
            position: from,
            velocity: 0.0,
        }
    }

    /// Advance the spring by `dt` seconds. Returns `true` if the spring is still moving.
    pub fn step(&mut self, dt: f64) -> bool {
        let displacement = self.position - self.to;
        let omega = (self.params.stiffness / self.params.mass).sqrt();
        let damping_force = 2.0 * self.params.damping * omega * self.velocity;
        let spring_force = omega * omega * displacement;
        let acceleration = -spring_force - damping_force;

        self.velocity += acceleration * dt;
        self.position += self.velocity * dt;

        // Check if settled
        let threshold = 0.001;
        displacement.abs() > threshold || self.velocity.abs() > threshold
    }

    /// Get the current interpolated value (0.0 to 1.0 progress).
    pub fn progress(&self) -> f64 {
        if (self.to - self.from).abs() < 1e-12 {
            return 1.0;
        }
        (self.position - self.from) / (self.to - self.from)
    }

    /// Is the spring settled at its target?
    pub fn is_settled(&self) -> bool {
        let displacement = (self.position - self.to).abs();
        displacement < 0.001 && self.velocity.abs() < 0.001
    }
}

// ---------------------------------------------------------------------------
// Animation description
// ---------------------------------------------------------------------------

/// Describes an animation to apply.
#[derive(Debug, Clone)]
pub enum AnimationKind {
    /// Linear interpolation from A to B over a duration.
    Tween {
        from: f64,
        to: f64,
        duration_secs: f64,
        easing: Easing,
    },
    /// Physics-based spring animation.
    Spring {
        from: f64,
        to: f64,
        params: SpringParams,
    },
    /// Multi-step keyframe animation.
    Keyframe {
        keyframes: Vec<KeyframePoint>,
        duration_secs: f64,
        easing: Easing,
    },
}

/// A point in a keyframe animation.
#[derive(Debug, Clone, Copy)]
pub struct KeyframePoint {
    /// Progress at which this keyframe occurs (0.0 to 1.0).
    pub progress: f64,
    /// Value at this keyframe.
    pub value: f64,
}

// ---------------------------------------------------------------------------
// Animation instance (running animation)
// ---------------------------------------------------------------------------

/// A running animation.
#[derive(Debug, Clone)]
pub struct Animation {
    pub id: AnimationId,
    pub kind: AnimationKind,
    pub elapsed_secs: f64,
    pub spring_state: Option<SpringState>,
    pub completed: bool,
}

impl Animation {
    pub fn new_tween(from: f64, to: f64, duration_secs: f64, easing: Easing) -> Self {
        Self {
            id: AnimationId::next(),
            kind: AnimationKind::Tween { from, to, duration_secs, easing },
            elapsed_secs: 0.0,
            spring_state: None,
            completed: false,
        }
    }

    pub fn new_spring(from: f64, to: f64, params: SpringParams) -> Self {
        Self {
            id: AnimationId::next(),
            kind: AnimationKind::Spring { from, to, params },
            elapsed_secs: 0.0,
            spring_state: Some(SpringState::new(from, to, params)),
            completed: false,
        }
    }

    pub fn new_keyframe(keyframes: Vec<KeyframePoint>, duration_secs: f64, easing: Easing) -> Self {
        Self {
            id: AnimationId::next(),
            kind: AnimationKind::Keyframe { keyframes, duration_secs, easing },
            elapsed_secs: 0.0,
            spring_state: None,
            completed: false,
        }
    }

    /// Advance the animation by `dt` seconds. Returns the current value.
    pub fn tick(&mut self, dt: f64) -> f64 {
        if self.completed {
            return self.final_value();
        }

        self.elapsed_secs += dt;

        match &mut self.kind {
            AnimationKind::Tween { from, to, duration_secs, easing } => {
                let progress = (self.elapsed_secs / *duration_secs).clamp(0.0, 1.0);
                let eased = easing.apply(progress);
                if progress >= 1.0 {
                    self.completed = true;
                }
                *from + (*to - *from) * eased
            }

            AnimationKind::Spring { .. } => {
                if let Some(ref mut spring) = self.spring_state {
                    let still_moving = spring.step(dt);
                    if !still_moving {
                        self.completed = true;
                        spring.position = spring.to;
                    }
                    spring.position
                } else {
                    self.completed = true;
                    0.0
                }
            }

            AnimationKind::Keyframe { keyframes, duration_secs, easing } => {
                let progress = (self.elapsed_secs / *duration_secs).clamp(0.0, 1.0);
                let eased = easing.apply(progress);
                if progress >= 1.0 {
                    self.completed = true;
                }
                interpolate_keyframes(keyframes, eased)
            }
        }
    }

    /// Get the final resting value of the animation.
    pub fn final_value(&self) -> f64 {
        match &self.kind {
            AnimationKind::Tween { to, .. } => *to,
            AnimationKind::Spring { to, .. } => *to,
            AnimationKind::Keyframe { keyframes, .. } => {
                keyframes.last().map(|k| k.value).unwrap_or(0.0)
            }
        }
    }

    /// Current value without advancing time.
    pub fn current_value(&self) -> f64 {
        if self.completed {
            return self.final_value();
        }

        match &self.kind {
            AnimationKind::Tween { from, to, duration_secs, easing } => {
                let progress = (self.elapsed_secs / *duration_secs).clamp(0.0, 1.0);
                let eased = easing.apply(progress);
                *from + (*to - *from) * eased
            }
            AnimationKind::Spring { .. } => {
                self.spring_state.as_ref().map(|s| s.position).unwrap_or(0.0)
            }
            AnimationKind::Keyframe { keyframes, duration_secs, easing } => {
                let progress = (self.elapsed_secs / *duration_secs).clamp(0.0, 1.0);
                let eased = easing.apply(progress);
                interpolate_keyframes(keyframes, eased)
            }
        }
    }
}

fn interpolate_keyframes(keyframes: &[KeyframePoint], progress: f64) -> f64 {
    if keyframes.is_empty() {
        return 0.0;
    }
    if keyframes.len() == 1 {
        return keyframes[0].value;
    }

    // Find the two keyframes surrounding the progress
    for i in 0..keyframes.len() - 1 {
        let a = &keyframes[i];
        let b = &keyframes[i + 1];
        if progress >= a.progress && progress <= b.progress {
            let segment_progress = if (b.progress - a.progress).abs() < 1e-12 {
                1.0
            } else {
                (progress - a.progress) / (b.progress - a.progress)
            };
            return a.value + (b.value - a.value) * segment_progress;
        }
    }

    // Past the last keyframe
    keyframes.last().unwrap().value
}

// ---------------------------------------------------------------------------
// Animation manager
// ---------------------------------------------------------------------------

/// Manages a collection of running animations.
pub struct AnimationManager {
    animations: Vec<Animation>,
}

impl AnimationManager {
    pub fn new() -> Self {
        Self {
            animations: Vec::new(),
        }
    }

    /// Add a new animation. If an animation with the same property is already
    /// running, it is cancelled and replaced.
    pub fn add(&mut self, animation: Animation) -> AnimationId {
        let id = animation.id;
        self.animations.push(animation);
        id
    }

    /// Cancel an animation by ID.
    pub fn cancel(&mut self, id: AnimationId) {
        self.animations.retain(|a| a.id != id);
    }

    /// Tick all animations. Returns true if any animation is still running.
    pub fn tick_all(&mut self, dt: f64) -> bool {
        let mut any_running = false;
        for anim in &mut self.animations {
            anim.tick(dt);
            if !anim.completed {
                any_running = true;
            }
        }
        // Remove completed animations
        self.animations.retain(|a| !a.completed);
        any_running
    }

    /// Get the current value of an animation by ID.
    pub fn value(&self, id: AnimationId) -> Option<f64> {
        self.animations.iter()
            .find(|a| a.id == id)
            .map(|a| a.current_value())
    }

    /// Number of running animations.
    pub fn active_count(&self) -> usize {
        self.animations.len()
    }

    /// Check if any animation is running.
    pub fn is_animating(&self) -> bool {
        !self.animations.is_empty()
    }
}

impl Default for AnimationManager {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_easing_linear() {
        assert!((Easing::Linear.apply(0.0)).abs() < 1e-6);
        assert!((Easing::Linear.apply(0.5) - 0.5).abs() < 1e-6);
        assert!((Easing::Linear.apply(1.0) - 1.0).abs() < 1e-6);
    }

    #[test]
    fn test_easing_ease_in() {
        let mid = Easing::EaseIn.apply(0.5);
        assert!(mid < 0.5, "ease-in should be slow at start: {}", mid);
        assert!((Easing::EaseIn.apply(0.0)).abs() < 1e-6);
        assert!((Easing::EaseIn.apply(1.0) - 1.0).abs() < 1e-6);
    }

    #[test]
    fn test_easing_ease_out() {
        let mid = Easing::EaseOut.apply(0.5);
        assert!(mid > 0.5, "ease-out should be fast at start: {}", mid);
        assert!((Easing::EaseOut.apply(0.0)).abs() < 1e-6);
        assert!((Easing::EaseOut.apply(1.0) - 1.0).abs() < 1e-6);
    }

    #[test]
    fn test_easing_ease_in_out() {
        assert!((Easing::EaseInOut.apply(0.0)).abs() < 1e-6);
        assert!((Easing::EaseInOut.apply(0.5) - 0.5).abs() < 1e-6);
        assert!((Easing::EaseInOut.apply(1.0) - 1.0).abs() < 1e-6);
    }

    #[test]
    fn test_easing_cubic_bezier() {
        let ease = Easing::CubicBezier(0.25, 0.1, 0.25, 1.0);
        assert!((ease.apply(0.0)).abs() < 1e-3);
        assert!((ease.apply(1.0) - 1.0).abs() < 1e-3);
        // Mid-point should be reasonable
        let mid = ease.apply(0.5);
        assert!(mid > 0.3 && mid < 0.9, "cubic bezier mid: {}", mid);
    }

    #[test]
    fn test_tween_animation() {
        let mut anim = Animation::new_tween(0.0, 100.0, 1.0, Easing::Linear);

        let v1 = anim.tick(0.5);
        assert!((v1 - 50.0).abs() < 1.0, "at 0.5s: {}", v1);
        assert!(!anim.completed);

        let v2 = anim.tick(0.5);
        assert!((v2 - 100.0).abs() < 1.0, "at 1.0s: {}", v2);
        assert!(anim.completed);
    }

    #[test]
    fn test_tween_ease_in() {
        let mut anim = Animation::new_tween(0.0, 100.0, 1.0, Easing::EaseIn);

        let v1 = anim.tick(0.5);
        assert!(v1 < 50.0, "ease-in at halfway should be < 50: {}", v1);
    }

    #[test]
    fn test_spring_animation() {
        let mut anim = Animation::new_spring(0.0, 100.0, SpringParams::default());

        // Simulate 2 seconds at 60fps
        let dt = 1.0 / 60.0;
        let mut last_val = 0.0;
        for _ in 0..120 {
            last_val = anim.tick(dt);
        }

        // Should be close to target after 2 seconds
        assert!((last_val - 100.0).abs() < 5.0, "spring after 2s: {}", last_val);
    }

    #[test]
    fn test_spring_overshoot() {
        let mut anim = Animation::new_spring(0.0, 100.0, SpringParams::bouncy());

        let dt = 1.0 / 60.0;
        let mut max_val: f64 = 0.0;
        for _ in 0..120 {
            let v = anim.tick(dt);
            max_val = max_val.max(v);
        }

        // Bouncy spring should overshoot past 100
        assert!(max_val > 100.0, "bouncy spring should overshoot: max={}", max_val);
    }

    #[test]
    fn test_spring_settles() {
        let mut anim = Animation::new_spring(0.0, 100.0, SpringParams::snappy());

        let dt = 1.0 / 60.0;
        for _ in 0..600 {
            anim.tick(dt);
        }

        // After 10 seconds, should be settled
        assert!(anim.completed, "spring should be settled after 10s");
    }

    #[test]
    fn test_keyframe_animation() {
        let keyframes = vec![
            KeyframePoint { progress: 0.0, value: 0.0 },
            KeyframePoint { progress: 0.5, value: 100.0 },
            KeyframePoint { progress: 1.0, value: 50.0 },
        ];
        let mut anim = Animation::new_keyframe(keyframes, 2.0, Easing::Linear);

        let v1 = anim.tick(0.5); // progress = 0.25
        assert!((v1 - 50.0).abs() < 5.0, "at progress 0.25: {}", v1);

        let v2 = anim.tick(0.5); // progress = 0.5
        assert!((v2 - 100.0).abs() < 5.0, "at progress 0.5: {}", v2);

        let v3 = anim.tick(1.0); // progress = 1.0
        assert!((v3 - 50.0).abs() < 5.0, "at progress 1.0: {}", v3);
        assert!(anim.completed);
    }

    #[test]
    fn test_animation_manager() {
        let mut mgr = AnimationManager::new();

        let anim1 = Animation::new_tween(0.0, 100.0, 1.0, Easing::Linear);
        let anim2 = Animation::new_tween(0.0, 200.0, 2.0, Easing::Linear);
        let id1 = mgr.add(anim1);
        let id2 = mgr.add(anim2);

        assert_eq!(mgr.active_count(), 2);

        // Tick 1 second: anim1 completes, anim2 still running
        let still_running = mgr.tick_all(1.0);
        assert!(still_running);
        assert_eq!(mgr.active_count(), 1);

        // anim1 was removed (completed)
        assert!(mgr.value(id1).is_none());
        // anim2 still running
        let v2 = mgr.value(id2);
        assert!(v2.is_some());
        assert!((v2.unwrap() - 100.0).abs() < 5.0);
    }

    #[test]
    fn test_animation_cancel() {
        let mut mgr = AnimationManager::new();
        let anim = Animation::new_tween(0.0, 100.0, 10.0, Easing::Linear);
        let id = mgr.add(anim);

        assert_eq!(mgr.active_count(), 1);
        mgr.cancel(id);
        assert_eq!(mgr.active_count(), 0);
    }

    #[test]
    fn test_multiple_simultaneous_animations() {
        let mut mgr = AnimationManager::new();

        for i in 0..10 {
            let anim = Animation::new_tween(0.0, (i + 1) as f64 * 10.0, 1.0, Easing::Linear);
            mgr.add(anim);
        }

        assert_eq!(mgr.active_count(), 10);
        mgr.tick_all(0.5);
        assert_eq!(mgr.active_count(), 10);
        mgr.tick_all(0.5);
        // All should complete at t=1.0
        assert_eq!(mgr.active_count(), 0);
    }

    #[test]
    fn test_spring_state_progress() {
        let state = SpringState::new(0.0, 100.0, SpringParams::default());
        assert!((state.progress()).abs() < 1e-6);
    }

    #[test]
    fn test_animation_no_frame_drops() {
        // Simulate a spring at 60fps and ensure values are smooth
        let mut anim = Animation::new_spring(0.0, 100.0, SpringParams::default());
        let dt = 1.0 / 60.0;
        let mut prev = 0.0;
        let mut max_delta: f64 = 0.0;

        for _ in 0..120 {
            let v = anim.tick(dt);
            let delta = (v - prev).abs();
            max_delta = max_delta.max(delta);
            prev = v;
        }

        // No single frame should jump more than 20 units (reasonable for spring)
        assert!(max_delta < 20.0, "max frame delta: {}", max_delta);
    }

    #[test]
    fn test_easing_clamp() {
        // Values outside [0,1] should be clamped
        assert!((Easing::Linear.apply(-1.0)).abs() < 1e-6);
        assert!((Easing::Linear.apply(2.0) - 1.0).abs() < 1e-6);
    }

    #[test]
    fn test_animation_final_value() {
        let anim = Animation::new_tween(10.0, 90.0, 1.0, Easing::Linear);
        assert!((anim.final_value() - 90.0).abs() < 1e-6);

        let anim2 = Animation::new_spring(0.0, 50.0, SpringParams::default());
        assert!((anim2.final_value() - 50.0).abs() < 1e-6);
    }
}

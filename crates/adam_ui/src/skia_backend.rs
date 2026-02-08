//! Skia rendering backend for Adam UI.
//!
//! Maps `RenderCommand` variants to Skia API calls using `skia-safe`.
//! Feature-gated behind the `skia` feature flag.
//!
//! Enable with: `cargo build --features skia`

#[cfg(feature = "skia")]
use skia_safe::{
    colors,
    textlayout::{FontCollection, ParagraphBuilder, ParagraphStyle, TextStyle},
    Canvas, ClipOp, Color4f, Font as SkFont, FontStyle, Paint, PaintStyle, Point, Rect, Surface,
    Typeface,
};

use crate::render::{RenderBackend, RenderCommand};
#[cfg(feature = "skia")]
use crate::vtree::Color;

/// Skia-based rendering backend.
///
/// Renders to an in-memory Skia surface that can be presented to a window
/// or saved as an image.
#[cfg(feature = "skia")]
pub struct SkiaRenderBackend {
    surface: Surface,
    width: i32,
    height: i32,
    clip_stack: Vec<()>,
    opacity_stack: Vec<f32>,
}

#[cfg(feature = "skia")]
impl SkiaRenderBackend {
    /// Create a new Skia backend with the given dimensions.
    pub fn new(width: i32, height: i32) -> Self {
        let surface =
            Surface::new_raster_n32_premul((width, height)).expect("failed to create Skia surface");
        Self {
            surface,
            width,
            height,
            clip_stack: Vec::new(),
            opacity_stack: Vec::new(),
        }
    }

    /// Get the underlying Skia surface for reading pixels, saving, etc.
    pub fn surface(&self) -> &Surface {
        &self.surface
    }

    /// Get mutable access to the surface.
    pub fn surface_mut(&mut self) -> &mut Surface {
        &mut self.surface
    }

    fn canvas(&mut self) -> &Canvas {
        self.surface.canvas()
    }

    fn current_opacity(&self) -> f32 {
        self.opacity_stack.last().copied().unwrap_or(1.0)
    }

    fn make_paint(&self, color: Color, style: PaintStyle) -> Paint {
        let alpha = (color.a as f32 * self.current_opacity()) as u8;
        let mut paint = Paint::default();
        paint.set_color(skia_safe::Color::from_argb(
            alpha, color.r, color.g, color.b,
        ));
        paint.set_style(style);
        paint.set_anti_alias(true);
        paint
    }

    fn layout_to_rect(rect: &LayoutRect) -> Rect {
        Rect::from_xywh(rect.x, rect.y, rect.width, rect.height)
    }

    fn execute_command(&mut self, command: &RenderCommand) {
        match command {
            RenderCommand::FillRect {
                rect,
                color,
                corner_radius,
            } => {
                let paint = self.make_paint(*color, PaintStyle::Fill);
                let sk_rect = Self::layout_to_rect(rect);
                if *corner_radius > 0.0 {
                    self.surface.canvas().draw_round_rect(
                        sk_rect,
                        *corner_radius,
                        *corner_radius,
                        &paint,
                    );
                } else {
                    self.surface.canvas().draw_rect(sk_rect, &paint);
                }
            }

            RenderCommand::StrokeRect {
                rect,
                color,
                width,
                corner_radius,
            } => {
                let mut paint = self.make_paint(*color, PaintStyle::Stroke);
                paint.set_stroke_width(*width);
                let sk_rect = Self::layout_to_rect(rect);
                if *corner_radius > 0.0 {
                    self.surface.canvas().draw_round_rect(
                        sk_rect,
                        *corner_radius,
                        *corner_radius,
                        &paint,
                    );
                } else {
                    self.surface.canvas().draw_rect(sk_rect, &paint);
                }
            }

            RenderCommand::DrawText {
                text,
                x,
                y,
                font,
                color,
            } => {
                let paint = self.make_paint(*color, PaintStyle::Fill);
                let typeface = Typeface::new("sans-serif", FontStyle::default())
                    .unwrap_or_else(Typeface::default);
                let sk_font = SkFont::new(typeface, Some(font.size));
                self.surface.canvas().draw_str(
                    text,
                    Point::new(*x, *y + font.size),
                    &sk_font,
                    &paint,
                );
            }

            RenderCommand::DrawImage { source: _, rect } => {
                // Image drawing would load the image from source and draw it.
                // Placeholder: draw a gray rect.
                let paint = self.make_paint(Color::rgb(200, 200, 200), PaintStyle::Fill);
                self.surface
                    .canvas()
                    .draw_rect(Self::layout_to_rect(rect), &paint);
            }

            RenderCommand::PushClip {
                rect,
                corner_radius,
            } => {
                self.surface.canvas().save();
                let sk_rect = Self::layout_to_rect(rect);
                if *corner_radius > 0.0 {
                    let rrect =
                        skia_safe::RRect::new_rect_xy(sk_rect, *corner_radius, *corner_radius);
                    self.surface
                        .canvas()
                        .clip_rrect(rrect, ClipOp::Intersect, true);
                } else {
                    self.surface
                        .canvas()
                        .clip_rect(sk_rect, ClipOp::Intersect, true);
                }
                self.clip_stack.push(());
            }

            RenderCommand::PopClip => {
                if self.clip_stack.pop().is_some() {
                    self.surface.canvas().restore();
                }
            }

            RenderCommand::PushOpacity(opacity) => {
                let effective = self.current_opacity() * opacity;
                self.opacity_stack.push(effective);
            }

            RenderCommand::PopOpacity => {
                self.opacity_stack.pop();
            }

            RenderCommand::DrawShadow {
                rect,
                shadow,
                corner_radius,
            } => {
                // Approximate shadow with a blurred, offset fill.
                let mut paint = self.make_paint(shadow.color, PaintStyle::Fill);
                let blur = skia_safe::MaskFilter::blur(
                    skia_safe::BlurStyle::Normal,
                    shadow.blur_radius / 2.0,
                    false,
                );
                paint.set_mask_filter(blur);
                let shadow_rect = Rect::from_xywh(
                    rect.x + shadow.offset_x,
                    rect.y + shadow.offset_y,
                    rect.width,
                    rect.height,
                );
                if *corner_radius > 0.0 {
                    self.surface.canvas().draw_round_rect(
                        shadow_rect,
                        *corner_radius,
                        *corner_radius,
                        &paint,
                    );
                } else {
                    self.surface.canvas().draw_rect(shadow_rect, &paint);
                }
            }

            RenderCommand::DrawProgress {
                rect,
                value,
                color,
                track_color,
            } => {
                // Track
                let track_paint = self.make_paint(*track_color, PaintStyle::Fill);
                let sk_rect = Self::layout_to_rect(rect);
                self.surface
                    .canvas()
                    .draw_round_rect(sk_rect, 4.0, 4.0, &track_paint);

                // Fill
                if let Some(v) = value {
                    let fill_width = (rect.width * *v as f32).min(rect.width);
                    let fill_rect = Rect::from_xywh(rect.x, rect.y, fill_width, rect.height);
                    let fill_paint = self.make_paint(*color, PaintStyle::Fill);
                    self.surface
                        .canvas()
                        .draw_round_rect(fill_rect, 4.0, 4.0, &fill_paint);
                }
            }

            RenderCommand::DrawToggle {
                rect,
                is_on,
                track_color,
                thumb_color,
            } => {
                // Track
                let track_paint = self.make_paint(*track_color, PaintStyle::Fill);
                let sk_rect = Self::layout_to_rect(rect);
                let radius = rect.height / 2.0;
                self.surface
                    .canvas()
                    .draw_round_rect(sk_rect, radius, radius, &track_paint);

                // Thumb
                let thumb_paint = self.make_paint(*thumb_color, PaintStyle::Fill);
                let thumb_r = radius - 2.0;
                let cx = if *is_on {
                    rect.x + rect.width - radius
                } else {
                    rect.x + radius
                };
                let cy = rect.y + radius;
                self.surface
                    .canvas()
                    .draw_circle(Point::new(cx, cy), thumb_r, &thumb_paint);
            }

            RenderCommand::DrawSlider {
                rect,
                value,
                min,
                max,
                track_color,
                thumb_color,
            } => {
                // Track
                let track_paint = self.make_paint(*track_color, PaintStyle::Fill);
                let track_y = rect.y + rect.height / 2.0 - 2.0;
                let track_rect = Rect::from_xywh(rect.x, track_y, rect.width, 4.0);
                self.surface
                    .canvas()
                    .draw_round_rect(track_rect, 2.0, 2.0, &track_paint);

                // Thumb
                let range = max - min;
                let pct = if range > 0.0 {
                    ((value - min) / range) as f32
                } else {
                    0.0
                };
                let cx = rect.x + pct * rect.width;
                let cy = rect.y + rect.height / 2.0;
                let thumb_paint = self.make_paint(*thumb_color, PaintStyle::Fill);
                self.surface
                    .canvas()
                    .draw_circle(Point::new(cx, cy), 10.0, &thumb_paint);
            }
        }
    }
}

#[cfg(feature = "skia")]
impl RenderBackend for SkiaRenderBackend {
    fn begin_frame(&mut self, width: f32, height: f32) {
        if width as i32 != self.width || height as i32 != self.height {
            self.width = width as i32;
            self.height = height as i32;
            self.surface = Surface::new_raster_n32_premul((self.width, self.height))
                .expect("failed to create Skia surface");
        }
        self.surface.canvas().clear(colors::WHITE);
        self.clip_stack.clear();
        self.opacity_stack.clear();
    }

    fn execute(&mut self, command: &RenderCommand) {
        self.execute_command(command);
    }

    fn end_frame(&mut self) {
        self.surface.flush();
    }
}

// ---------------------------------------------------------------------------
// Non-skia stub (always compiled)
// ---------------------------------------------------------------------------

/// Stub backend available when the `skia` feature is disabled.
/// Returns an error message directing users to enable the feature.
#[cfg(not(feature = "skia"))]
pub struct SkiaRenderBackend;

#[cfg(not(feature = "skia"))]
impl SkiaRenderBackend {
    pub fn new(_width: i32, _height: i32) -> Self {
        panic!(
            "Skia backend requires the `skia` feature. Enable with: cargo build --features skia"
        );
    }
}

#[cfg(not(feature = "skia"))]
impl RenderBackend for SkiaRenderBackend {
    fn begin_frame(&mut self, _width: f32, _height: f32) {}
    fn execute(&mut self, _command: &RenderCommand) {}
    fn end_frame(&mut self) {}
}

// ---------------------------------------------------------------------------
// Tests (run without skia feature — test the stub)
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[cfg(not(feature = "skia"))]
    #[should_panic(expected = "Skia backend requires the `skia` feature")]
    fn test_skia_stub_panics() {
        let _backend = SkiaRenderBackend::new(400, 800);
    }
}

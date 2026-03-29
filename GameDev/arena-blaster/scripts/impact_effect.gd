extends Node2D
class_name ImpactEffect

var effect_color := Color.WHITE
var start_radius := 20.0
var duration := 0.22
var life_left := 0.22


func setup(at_position: Vector2, color: Color, radius: float = 20.0, effect_duration: float = 0.22) -> void:
	global_position = at_position
	effect_color = color
	start_radius = radius
	duration = effect_duration
	life_left = effect_duration
	queue_redraw()


func _process(delta: float) -> void:
	life_left -= delta
	if life_left <= 0.0:
		queue_free()
		return
	queue_redraw()


func _draw() -> void:
	var t := 1.0 - life_left / maxf(duration, 0.001)
	var radius := lerpf(start_radius * 0.4, start_radius * 1.25, t)
	var alpha := 1.0 - t

	draw_circle(Vector2.ZERO, radius, Color(effect_color, 0.32 * alpha))
	draw_circle(Vector2.ZERO, radius * 0.45, Color(1.0, 1.0, 1.0, 0.7 * alpha))

extends Node2D
class_name Buff

var buff_type: StringName = &"speed"
var radius := 18.0
var lifetime := 10.0
var pulse_time := 0.0


func setup(type_name: StringName) -> void:
	buff_type = type_name
	queue_redraw()


func _process(delta: float) -> void:
	pulse_time += delta
	lifetime -= delta
	if lifetime <= 0.0:
		queue_free()
		return

	rotation += delta * 0.9
	queue_redraw()


func get_collision_radius() -> float:
	return radius


func _draw() -> void:
	var pulse := 1.0 + 0.1 * sin(pulse_time * 7.0)
	var main_color := _get_color()

	draw_circle(Vector2.ZERO, radius * pulse, Color(main_color, 0.2))
	draw_circle(Vector2.ZERO, radius * 0.72 * pulse, main_color)

	match buff_type:
		&"speed":
			draw_line(Vector2(-8, 6), Vector2(0, -8), Color.WHITE, 3.0)
			draw_line(Vector2(0, -8), Vector2(8, 6), Color.WHITE, 3.0)
		&"fire_rate":
			draw_circle(Vector2.ZERO, 4.0, Color.WHITE)
			draw_line(Vector2(-9, 0), Vector2(9, 0), Color.WHITE, 2.0)
		&"shield":
			draw_rect(Rect2(Vector2(-6, -8), Vector2(12, 16)), Color.WHITE, false, 2.0)


func _get_color() -> Color:
	match buff_type:
		&"speed":
			return Color(0.3, 1.0, 0.4, 0.95)
		&"fire_rate":
			return Color(1.0, 0.7, 0.25, 0.95)
		&"shield":
			return Color(0.35, 0.9, 1.0, 0.95)
	return Color(0.9, 0.9, 0.9, 0.95)

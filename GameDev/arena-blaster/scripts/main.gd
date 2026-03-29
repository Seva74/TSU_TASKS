extends Node2D

const ARENA_SIZE := Vector2(1920.0, 1080.0)
const ARENA_RECT := Rect2(Vector2.ZERO, ARENA_SIZE)
const MAX_WAVES := 5
const BASE_WAVE_ENEMIES := 6
const ENEMIES_PER_WAVE_STEP := 5
const BUFF_DROP_CHANCE := 0.14
const WALL_LAYER_MASK := 1 << 1
const COVER_QUERY_MASK := 1 << 1
const SAVE_PATH := "user://save_data.cfg"

const COMBO_TIMEOUT := 2.8
const COMBO_STEP := 0.15
const MAX_COMBO_MULTIPLIER := 2.2

const SHAKE_DECAY := 30.0
const MAX_SHAKE := 10.0
const HEAL_AMOUNT := 35.0

const PLAYER_SCRIPT := preload("res://scripts/player.gd")
const ENEMY_SCRIPT := preload("res://scripts/enemy.gd")
const PROJECTILE_SCRIPT := preload("res://scripts/projectile.gd")
const BUFF_SCRIPT := preload("res://scripts/buff.gd")
const IMPACT_EFFECT_SCRIPT := preload("res://scripts/impact_effect.gd")
const AUDIO_MANAGER_SCRIPT := preload("res://scripts/audio_manager.gd")

enum GameState {
	MENU,
	PLAYING,
	WEAPON_CHOICE,
	FINISHED
}

var rng := RandomNumberGenerator.new()
var state: GameState = GameState.MENU

var world_root: Node2D
var map_root: Node2D
var entity_root: Node2D
var effects_root: Node2D
var gameplay_camera: Camera2D

var ui_layer: CanvasLayer
var menu_root: Control
var hud_root: Control
var weapon_choice_root: Control
var end_root: Control
var pause_root: Control

var menu_best_label: Label

var hp_bar: ProgressBar
var hp_label: Label
var shield_label: Label
var score_label: Label
var wave_label: Label
var enemies_label: Label
var buff_label: Label
var weapon_label: Label
var armor_label: Label
var adrenaline_label: Label
var combo_label: Label
var dash_label: Label
var wave_banner: Label

var end_title_label: Label
var end_score_label: Label
var end_best_label: Label

var player: Player
var audio_manager: AudioManager

var enemies: Array[Enemy] = []
var projectiles: Array[Projectile] = []
var buffs: Array[Buff] = []
var cover_rects: Array[Rect2] = []

var score := 0
var current_wave := 0
var pending_spawns := 0
var spawn_timer := 0.0
var spawn_interval := 0.2
var boss_spawned := false

var between_waves := false
var between_waves_left := 0.0

var run_finished := false
var run_victory := false
var is_paused := false

var best_score := 0
var combo_count := 0
var combo_multiplier := 1.0
var combo_time_left := 0.0
var screen_shake := 0.0
var weapon_choice_active := false
var pending_weapon_choice_victory := false
var weapon_choice_options: Array[StringName] = []

var wave_banner_left := 0.0


func _ready() -> void:
	rng.randomize()
	process_mode = Node.PROCESS_MODE_ALWAYS
	_configure_input_map()
	DisplayServer.window_set_mode(DisplayServer.WINDOW_MODE_FULLSCREEN)
	_load_persistent_data()

	_build_ui()
	_show_menu()
	queue_redraw()


func _draw() -> void:
	if state == GameState.MENU:
		return

	draw_rect(ARENA_RECT, Color(0.05, 0.08, 0.11), true)

	for cover_rect in cover_rects:
		draw_rect(cover_rect, Color(0.12, 0.16, 0.2, 0.92), true)
		draw_rect(cover_rect.grow(2.0), Color(0.58, 0.72, 0.95, 0.48), false, 2.0)

	var grid_color := Color(0.2, 0.24, 0.3, 0.34)
	for x in range(64, int(ARENA_SIZE.x), 64):
		draw_line(Vector2(x, 0), Vector2(x, ARENA_SIZE.y), grid_color, 1.0)
	for y in range(64, int(ARENA_SIZE.y), 64):
		draw_line(Vector2(0, y), Vector2(ARENA_SIZE.x, y), grid_color, 1.0)

	draw_rect(ARENA_RECT, Color(0.65, 0.8, 1.0, 0.95), false, 5.0)


func _process(delta: float) -> void:
	if is_paused or weapon_choice_active:
		return

	if combo_time_left > 0.0:
		combo_time_left -= delta
		if combo_time_left <= 0.0:
			_reset_combo()

	_update_screen_shake(delta)

	if wave_banner_left > 0.0:
		wave_banner_left -= delta
		if wave_banner_left <= 0.0:
			wave_banner.visible = false
		else:
			if wave_banner_left < 0.35:
				wave_banner.modulate.a = wave_banner_left / 0.35
			else:
				wave_banner.modulate.a = 1.0


func _physics_process(delta: float) -> void:
	if state != GameState.PLAYING or run_finished or is_paused or weapon_choice_active:
		return

	_handle_spawning(delta)
	_handle_wave_transitions(delta)
	_handle_projectile_collisions()
	_handle_contact_damage()
	_handle_buff_pickups()
	_compact_runtime_arrays()
	_update_hud()


func _build_ui() -> void:
	ui_layer = CanvasLayer.new()
	add_child(ui_layer)

	menu_root = _create_menu_ui()
	hud_root = _create_hud_ui()
	weapon_choice_root = _create_weapon_choice_ui()
	end_root = _create_end_ui()
	pause_root = _create_pause_ui()

	ui_layer.add_child(menu_root)
	ui_layer.add_child(hud_root)
	ui_layer.add_child(weapon_choice_root)
	ui_layer.add_child(end_root)
	ui_layer.add_child(pause_root)

	hud_root.visible = false
	weapon_choice_root.visible = false
	end_root.visible = false
	pause_root.visible = false
	_refresh_best_score_labels()


func _create_menu_ui() -> Control:
	var root := Control.new()
	root.set_anchors_and_offsets_preset(Control.PRESET_FULL_RECT)

	var overlay := ColorRect.new()
	overlay.set_anchors_and_offsets_preset(Control.PRESET_FULL_RECT)
	overlay.color = Color(0.02, 0.03, 0.05, 0.96)
	root.add_child(overlay)

	var panel := PanelContainer.new()
	panel.custom_minimum_size = Vector2(560, 360)
	panel.position = Vector2(680, 290)
	root.add_child(panel)

	var vbox := VBoxContainer.new()
	vbox.alignment = BoxContainer.ALIGNMENT_CENTER
	vbox.add_theme_constant_override("separation", 18)
	panel.add_child(vbox)

	var title := Label.new()
	title.text = "ARENA BLASTER"
	title.horizontal_alignment = HORIZONTAL_ALIGNMENT_CENTER
	title.add_theme_font_size_override("font_size", 56)
	vbox.add_child(title)

	var subtitle := Label.new()
	subtitle.text = "Выживи, выбирай оружие после босса и прячься за укрытиями"
	subtitle.horizontal_alignment = HORIZONTAL_ALIGNMENT_CENTER
	subtitle.add_theme_font_size_override("font_size", 22)
	vbox.add_child(subtitle)

	menu_best_label = Label.new()
	menu_best_label.horizontal_alignment = HORIZONTAL_ALIGNMENT_CENTER
	menu_best_label.add_theme_font_size_override("font_size", 24)
	vbox.add_child(menu_best_label)

	var start_button := Button.new()
	start_button.text = "START"
	start_button.custom_minimum_size = Vector2(260, 60)
	start_button.pressed.connect(_start_new_run)
	vbox.add_child(start_button)

	var exit_button := Button.new()
	exit_button.text = "EXIT"
	exit_button.custom_minimum_size = Vector2(260, 54)
	exit_button.pressed.connect(_exit_game)
	vbox.add_child(exit_button)

	return root


func _create_hud_ui() -> Control:
	var root := Control.new()
	root.set_anchors_and_offsets_preset(Control.PRESET_FULL_RECT)

	var panel := PanelContainer.new()
	panel.position = Vector2(20, 16)
	panel.custom_minimum_size = Vector2(700, 150)
	root.add_child(panel)

	var hud_box := VBoxContainer.new()
	hud_box.add_theme_constant_override("separation", 6)
	panel.add_child(hud_box)

	hp_label = Label.new()
	hp_label.text = "HP"
	hud_box.add_child(hp_label)

	hp_bar = ProgressBar.new()
	hp_bar.custom_minimum_size = Vector2(640, 20)
	hp_bar.show_percentage = false
	hud_box.add_child(hp_bar)

	shield_label = Label.new()
	shield_label.text = "Щит: 0"
	hud_box.add_child(shield_label)

	var stats_line := HBoxContainer.new()
	stats_line.add_theme_constant_override("separation", 22)
	hud_box.add_child(stats_line)

	score_label = Label.new()
	score_label.text = "Очки: 0"
	stats_line.add_child(score_label)

	wave_label = Label.new()
	wave_label.text = "Волна: 0/5"
	stats_line.add_child(wave_label)

	enemies_label = Label.new()
	enemies_label.text = "Враги: 0"
	stats_line.add_child(enemies_label)

	buff_label = Label.new()
	buff_label.position = Vector2(20, 172)
	buff_label.text = "Баффы: нет"
	root.add_child(buff_label)

	weapon_label = Label.new()
	weapon_label.position = Vector2(20, 196)
	weapon_label.text = "Оружие: Пистолет"
	root.add_child(weapon_label)

	armor_label = Label.new()
	armor_label.position = Vector2(20, 220)
	armor_label.text = "Броня: 0"
	root.add_child(armor_label)

	adrenaline_label = Label.new()
	adrenaline_label.position = Vector2(20, 244)
	adrenaline_label.text = "Адреналин: нет"
	root.add_child(adrenaline_label)

	combo_label = Label.new()
	combo_label.position = Vector2(20, 268)
	combo_label.text = "Комбо: x1.00"
	root.add_child(combo_label)

	dash_label = Label.new()
	dash_label.position = Vector2(20, 292)
	dash_label.text = "Рывок: READY"
	root.add_child(dash_label)

	var hint_label := Label.new()
	hint_label.position = Vector2(1110, 16)
	hint_label.text = "Мышь: стрельба | R: перезарядка | SPACE/SHIFT: рывок | ESC: пауза"
	root.add_child(hint_label)

	wave_banner = Label.new()
	wave_banner.horizontal_alignment = HORIZONTAL_ALIGNMENT_CENTER
	wave_banner.vertical_alignment = VERTICAL_ALIGNMENT_CENTER
	wave_banner.size = Vector2(500, 60)
	wave_banner.position = Vector2(710, 30)
	wave_banner.add_theme_font_size_override("font_size", 40)
	wave_banner.visible = false
	root.add_child(wave_banner)

	return root


func _create_end_ui() -> Control:
	var root := Control.new()
	root.set_anchors_and_offsets_preset(Control.PRESET_FULL_RECT)

	var overlay := ColorRect.new()
	overlay.set_anchors_and_offsets_preset(Control.PRESET_FULL_RECT)
	overlay.color = Color(0.02, 0.02, 0.03, 0.8)
	root.add_child(overlay)

	var panel := PanelContainer.new()
	panel.custom_minimum_size = Vector2(640, 330)
	panel.position = Vector2(640, 300)
	root.add_child(panel)

	var box := VBoxContainer.new()
	box.alignment = BoxContainer.ALIGNMENT_CENTER
	box.add_theme_constant_override("separation", 14)
	panel.add_child(box)

	end_title_label = Label.new()
	end_title_label.horizontal_alignment = HORIZONTAL_ALIGNMENT_CENTER
	end_title_label.add_theme_font_size_override("font_size", 52)
	box.add_child(end_title_label)

	end_score_label = Label.new()
	end_score_label.horizontal_alignment = HORIZONTAL_ALIGNMENT_CENTER
	end_score_label.add_theme_font_size_override("font_size", 28)
	box.add_child(end_score_label)

	end_best_label = Label.new()
	end_best_label.horizontal_alignment = HORIZONTAL_ALIGNMENT_CENTER
	end_best_label.add_theme_font_size_override("font_size", 24)
	box.add_child(end_best_label)

	var restart_button := Button.new()
	restart_button.text = "RESTART"
	restart_button.custom_minimum_size = Vector2(280, 58)
	restart_button.pressed.connect(_start_new_run)
	box.add_child(restart_button)

	var menu_button := Button.new()
	menu_button.text = "MENU"
	menu_button.custom_minimum_size = Vector2(280, 52)
	menu_button.pressed.connect(_show_menu)
	box.add_child(menu_button)

	return root


func _create_pause_ui() -> Control:
	var root := Control.new()
	root.set_anchors_and_offsets_preset(Control.PRESET_FULL_RECT)

	var overlay := ColorRect.new()
	overlay.set_anchors_and_offsets_preset(Control.PRESET_FULL_RECT)
	overlay.color = Color(0.0, 0.0, 0.0, 0.52)
	root.add_child(overlay)

	var panel := PanelContainer.new()
	panel.custom_minimum_size = Vector2(460, 260)
	panel.position = Vector2(730, 340)
	root.add_child(panel)

	var box := VBoxContainer.new()
	box.alignment = BoxContainer.ALIGNMENT_CENTER
	box.add_theme_constant_override("separation", 12)
	panel.add_child(box)

	var title := Label.new()
	title.text = "ПАУЗА"
	title.horizontal_alignment = HORIZONTAL_ALIGNMENT_CENTER
	title.add_theme_font_size_override("font_size", 44)
	box.add_child(title)

	var continue_button := Button.new()
	continue_button.text = "ПРОДОЛЖИТЬ"
	continue_button.custom_minimum_size = Vector2(280, 56)
	continue_button.pressed.connect(Callable(self, "_set_pause_enabled").bind(false))
	box.add_child(continue_button)

	var menu_button := Button.new()
	menu_button.text = "В МЕНЮ"
	menu_button.custom_minimum_size = Vector2(280, 52)
	menu_button.pressed.connect(_on_pause_menu_pressed)
	box.add_child(menu_button)

	return root


func _create_weapon_choice_ui() -> Control:
	var root := Control.new()
	root.set_anchors_and_offsets_preset(Control.PRESET_FULL_RECT)
	root.process_mode = Node.PROCESS_MODE_ALWAYS

	var overlay := ColorRect.new()
	overlay.set_anchors_and_offsets_preset(Control.PRESET_FULL_RECT)
	overlay.color = Color(0.02, 0.02, 0.03, 0.9)
	root.add_child(overlay)

	var panel := PanelContainer.new()
	panel.custom_minimum_size = Vector2(780, 420)
	panel.position = Vector2(570, 280)
	root.add_child(panel)

	var box := VBoxContainer.new()
	box.alignment = BoxContainer.ALIGNMENT_CENTER
	box.add_theme_constant_override("separation", 16)
	panel.add_child(box)

	var title := Label.new()
	title.horizontal_alignment = HORIZONTAL_ALIGNMENT_CENTER
	title.add_theme_font_size_override("font_size", 42)
	title.text = "ВЫБОР ОРУЖИЯ"
	box.add_child(title)

	var subtitle := Label.new()
	subtitle.horizontal_alignment = HORIZONTAL_ALIGNMENT_CENTER
	subtitle.add_theme_font_size_override("font_size", 20)
	subtitle.text = "После босса выбери оружие или оставь текущее"
	box.add_child(subtitle)

	for index in range(3):
		var button := Button.new()
		button.custom_minimum_size = Vector2(520, 86)
		button.autowrap_mode = TextServer.AUTOWRAP_WORD_SMART
		button.alignment = HORIZONTAL_ALIGNMENT_LEFT
		button.pressed.connect(Callable(self, "_on_weapon_choice_button_pressed").bind(index))
		box.add_child(button)

	return root


func _show_menu() -> void:
	_set_pause_enabled(false)
	_clear_run_world()
	weapon_choice_active = false
	pending_weapon_choice_victory = false
	weapon_choice_options.clear()
	if weapon_choice_root:
		weapon_choice_root.visible = false
	state = GameState.MENU
	menu_root.visible = true
	hud_root.visible = false
	end_root.visible = false
	_refresh_best_score_labels()
	queue_redraw()


func _start_new_run() -> void:
	_set_pause_enabled(false)
	_clear_run_world()
	weapon_choice_active = false
	pending_weapon_choice_victory = false
	weapon_choice_options.clear()
	if weapon_choice_root:
		weapon_choice_root.visible = false

	state = GameState.PLAYING
	run_finished = false
	run_victory = false
	score = 0
	current_wave = 0
	pending_spawns = 0
	boss_spawned = false
	between_waves = false
	between_waves_left = 0.0
	screen_shake = 0.0
	_reset_combo()

	menu_root.visible = false
	hud_root.visible = true
	weapon_choice_root.visible = false
	end_root.visible = false

	world_root = Node2D.new()
	world_root.name = "World"
	world_root.process_mode = Node.PROCESS_MODE_PAUSABLE
	add_child(world_root)

	gameplay_camera = Camera2D.new()
	gameplay_camera.enabled = true
	gameplay_camera.position = ARENA_SIZE * 0.5
	gameplay_camera.process_mode = Node.PROCESS_MODE_PAUSABLE
	world_root.add_child(gameplay_camera)

	map_root = Node2D.new()
	map_root.name = "Map"
	world_root.add_child(map_root)
	_build_arena_map()

	entity_root = Node2D.new()
	entity_root.name = "Entities"
	world_root.add_child(entity_root)

	effects_root = Node2D.new()
	effects_root.name = "Effects"
	world_root.add_child(effects_root)

	audio_manager = AUDIO_MANAGER_SCRIPT.new()
	add_child(audio_manager)

	player = PLAYER_SCRIPT.new()
	player.global_position = ARENA_SIZE * 0.5
	player.set_arena_size(ARENA_SIZE)
	player.collision_mask |= WALL_LAYER_MASK
	player.set_weapon_type(&"pistol")
	player.shoot_requested.connect(_on_player_shoot_requested)
	player.health_changed.connect(_on_player_health_changed)
	player.died.connect(_on_player_died)
	entity_root.add_child(player)

	_start_wave(1)
	_update_hud()
	queue_redraw()


func _clear_run_world() -> void:
	enemies.clear()
	projectiles.clear()
	buffs.clear()
	cover_rects.clear()

	if is_instance_valid(world_root):
		world_root.queue_free()
	world_root = null
	map_root = null
	entity_root = null
	effects_root = null
	gameplay_camera = null
	player = null

	if is_instance_valid(audio_manager):
		audio_manager.queue_free()
	audio_manager = null


func _exit_game() -> void:
	get_tree().quit()


func _start_wave(wave_number: int) -> void:
	current_wave = wave_number
	pending_spawns = BASE_WAVE_ENEMIES + ENEMIES_PER_WAVE_STEP * (wave_number - 1)
	spawn_interval = maxf(0.12, 0.26 - 0.02 * float(wave_number - 1))
	spawn_timer = 0.0
	boss_spawned = false
	between_waves = false
	between_waves_left = 0.0

	_show_banner("ВОЛНА %d" % wave_number, 1.8)
	if is_instance_valid(audio_manager):
		audio_manager.play_wave_start()


func _handle_spawning(delta: float) -> void:
	if pending_spawns <= 0:
		return

	spawn_timer -= delta
	if spawn_timer > 0.0:
		return

	spawn_timer = spawn_interval
	pending_spawns -= 1

	var enemy_type: StringName = &"melee"
	var roll := rng.randf()
	if current_wave >= 2 and roll > 0.67:
		enemy_type = &"ranged"
	if current_wave >= 4 and roll > 0.84:
		enemy_type = &"ranged"

	_spawn_enemy(enemy_type)


func _handle_wave_transitions(delta: float) -> void:
	if weapon_choice_active:
		return

	if pending_spawns > 0:
		return

	if not enemies.is_empty():
		return

	if not boss_spawned:
		boss_spawned = true
		_spawn_enemy(&"boss")
		_show_banner("БОСС ВОЛНЫ", 1.2)
		return

	if current_wave >= MAX_WAVES:
		_finish_run(true)
		return

	if not between_waves:
		between_waves = true
		between_waves_left = 2.0
		_show_banner("ВОЛНА %d ОЧИЩЕНА" % current_wave, 1.2)
		return

	between_waves_left -= delta
	if between_waves_left <= 0.0:
		_start_wave(current_wave + 1)


func _spawn_enemy(enemy_type: StringName) -> void:
	if not is_instance_valid(entity_root):
		return

	var enemy: Enemy = ENEMY_SCRIPT.new()
	enemy.configure(enemy_type)
	if enemy_type != &"boss":
		var elite_chance := 0.0
		if current_wave >= 3:
			elite_chance = 0.12 + float(current_wave - 3) * 0.05
		if rng.randf() < elite_chance:
			enemy.make_elite()
	enemy.set_arena_size(ARENA_SIZE)
	enemy.collision_mask |= WALL_LAYER_MASK
	enemy.target = player
	enemy.visibility_provider = Callable(self, "_has_clear_sight_to_player").bind(enemy)
	enemy.global_position = _get_spawn_point_on_edge(enemy_type == &"boss")
	enemy.died.connect(_on_enemy_died)
	enemy.projectile_requested.connect(_on_enemy_projectile_requested)

	entity_root.add_child(enemy)
	enemies.append(enemy)


func _get_spawn_point_on_edge(is_boss: bool) -> Vector2:
	var margin := 36.0
	if is_boss:
		margin = 70.0

	match rng.randi_range(0, 3):
		0:
			return Vector2(rng.randf_range(0.0, ARENA_SIZE.x), -margin)
		1:
			return Vector2(ARENA_SIZE.x + margin, rng.randf_range(0.0, ARENA_SIZE.y))
		2:
			return Vector2(rng.randf_range(0.0, ARENA_SIZE.x), ARENA_SIZE.y + margin)
		_:
			return Vector2(-margin, rng.randf_range(0.0, ARENA_SIZE.y))


func _on_player_shoot_requested(origin: Vector2, direction: Vector2, damage: float, weapon_type: StringName) -> void:
	var weapon_config := player.get_weapon_config() if is_instance_valid(player) else {}
	if weapon_type == &"axe":
		var melee_range := float(weapon_config.get("melee_range", 90.0))
		var melee_arc := float(weapon_config.get("melee_arc", 1.1))
		var hits := 0
		for enemy in enemies:
			if not is_instance_valid(enemy):
				continue
			var to_enemy := enemy.global_position - origin
			if to_enemy.length() > melee_range:
				continue
			if absf(direction.angle_to(to_enemy.normalized())) > melee_arc * 0.5:
				continue
			enemy.take_damage(damage)
			_spawn_impact(enemy.global_position, Color(1.0, 0.72, 0.38), 22.0)
			_push_screen_shake(1.4)
			hits += 1
		if hits > 0 and is_instance_valid(audio_manager):
			audio_manager.play_shoot()
		return

	var projectile_speed := float(weapon_config.get("projectile_speed", 940.0))
	var projectile_lifetime := float(weapon_config.get("projectile_lifetime", 1.7))
	_spawn_projectile(origin + direction * 18.0, direction, damage, projectile_speed, false, weapon_type, projectile_lifetime)
	if is_instance_valid(audio_manager):
		audio_manager.play_shoot()


func _on_enemy_projectile_requested(origin: Vector2, direction: Vector2, damage: float, speed: float, _from_enemy: bool) -> void:
	_spawn_projectile(origin + direction * 18.0, direction, damage, speed, true)
	if is_instance_valid(audio_manager):
		audio_manager.play_enemy_shoot()


func _spawn_projectile(origin: Vector2, direction: Vector2, damage: float, speed: float, from_enemy: bool, weapon_type: StringName = &"pistol", lifetime_override: float = -1.0) -> void:
	if not is_instance_valid(entity_root):
		return

	var projectile: Projectile = PROJECTILE_SCRIPT.new()
	projectile.setup(origin, direction, speed, damage, from_enemy, weapon_type, lifetime_override)
	entity_root.add_child(projectile)
	projectiles.append(projectile)


func _handle_projectile_collisions() -> void:
	if not is_instance_valid(player):
		return

	for projectile in projectiles:
		if not is_instance_valid(projectile):
			continue

		var projectile_radius: float = float(projectile.radius)

		if projectile.is_outside(ARENA_RECT, 60.0):
			projectile.queue_free()
			continue

		if _projectile_hits_wall(projectile):
			_spawn_impact(projectile.global_position, Color(0.78, 0.82, 0.94), 14.0)
			projectile.queue_free()
			continue

		if projectile.from_enemy:
			var player_hit_distance: float = player.get_collision_radius() + projectile_radius
			if player.global_position.distance_to(projectile.global_position) <= player_hit_distance:
				if not player.is_dash_invulnerable():
					player.apply_damage(projectile.damage)
					_reset_combo()
					_spawn_impact(projectile.global_position, Color(1.0, 0.45, 0.45), 18.0)
					_push_screen_shake(4.0)
					if is_instance_valid(audio_manager):
						audio_manager.play_hurt()
				projectile.queue_free()
		else:
			for enemy in enemies:
				if not is_instance_valid(enemy):
					continue
				if projectile.has_hit_enemy(enemy):
					continue

				var enemy_hit_distance: float = float(enemy.get_collision_radius()) + projectile_radius
				if enemy.global_position.distance_to(projectile.global_position) <= enemy_hit_distance:
					enemy.take_damage(projectile.damage)
					projectile.register_enemy_hit(enemy)
					_spawn_impact(projectile.global_position, Color(0.35, 0.95, 1.0), 14.0)
					_push_screen_shake(1.2)
					if projectile.pierce_remaining > 0:
						projectile.pierce_remaining -= 1
						continue
					projectile.queue_free()
					break


func _handle_contact_damage() -> void:
	if not is_instance_valid(player):
		return

	for enemy in enemies:
		if not is_instance_valid(enemy):
			continue

		var hit_distance: float = float(enemy.get_collision_radius()) + player.get_collision_radius() - 2.0
		if player.global_position.distance_to(enemy.global_position) <= hit_distance:
			if enemy.can_deal_contact_damage():
				enemy.register_contact_damage()
				if not player.is_dash_invulnerable():
					player.apply_damage(enemy.contact_damage)
					_reset_combo()
					_spawn_impact(player.global_position, Color(1.0, 0.5, 0.4), 20.0)
					_push_screen_shake(4.5)
					if is_instance_valid(audio_manager):
						audio_manager.play_hurt()


func _on_enemy_died(enemy_type: StringName, score_gain: int, death_position: Vector2) -> void:
	var awarded_score := _calculate_combo_score(score_gain)
	score += awarded_score
	_spawn_impact(death_position, Color(1.0, 0.7, 0.35), 22.0)
	_push_screen_shake(6.0 if enemy_type == &"boss" else 3.0)
	if is_instance_valid(audio_manager):
		audio_manager.play_explosion()

	if enemy_type == &"boss":
		_spawn_buff(death_position + Vector2(0, -16), &"health")
		if rng.randf() < 0.75:
			_spawn_buff(death_position + Vector2(14, 12), &"armor")
		if is_instance_valid(player) and bool(player.get_weapon_config().get("uses_ammo", true)):
			_spawn_buff(death_position + Vector2(-16, -12), &"ammo")
		if rng.randf() < 0.85:
			_spawn_buff(death_position + Vector2(22, -10))
		if rng.randf() < 0.65:
			_spawn_buff(death_position + Vector2(-24, 14))
		if current_wave < MAX_WAVES:
			_open_weapon_choice()
	elif rng.randf() < BUFF_DROP_CHANCE:
		if is_instance_valid(player) and bool(player.get_weapon_config().get("uses_ammo", true)) and rng.randf() < 0.4:
			_spawn_buff(death_position, &"ammo")
		else:
			_spawn_buff(death_position)
	elif rng.randf() < 0.12:
		_spawn_buff(death_position, &"health")

	if combo_count >= 3 and awarded_score > score_gain:
		var combo_message := ""
		if combo_count >= 10:
			combo_message = "MEGA KILL STREAK! x%.2f" % combo_multiplier
		elif combo_count >= 7:
			combo_message = "KILL RAMPAGE! x%.2f" % combo_multiplier
		elif combo_count >= 5:
			combo_message = "KILL STREAK! x%.2f" % combo_multiplier
		else:
			combo_message = "КОМБО x%.2f" % combo_multiplier
		_show_banner(combo_message, 0.45)

	if is_instance_valid(player) and combo_count >= 3:
		player.apply_adrenaline(3.5)


func _build_arena_map() -> void:
	cover_rects = [
		Rect2(Vector2(350, 220), Vector2(250, 44)),
		Rect2(Vector2(1320, 220), Vector2(250, 44)),
		Rect2(Vector2(350, 816), Vector2(250, 44)),
		Rect2(Vector2(1320, 816), Vector2(250, 44)),
		Rect2(Vector2(910, 360), Vector2(100, 360)),
		Rect2(Vector2(620, 520), Vector2(170, 36)),
		Rect2(Vector2(1130, 520), Vector2(170, 36))
	]

	for cover_rect in cover_rects:
		var cover_body := StaticBody2D.new()
		cover_body.collision_layer = WALL_LAYER_MASK
		cover_body.position = cover_rect.position + cover_rect.size * 0.5
		map_root.add_child(cover_body)

		var cover_shape := CollisionShape2D.new()
		var rectangle := RectangleShape2D.new()
		rectangle.size = cover_rect.size
		cover_shape.shape = rectangle
		cover_body.add_child(cover_shape)


func _has_clear_sight_to_player(enemy: Enemy) -> bool:
	if not is_instance_valid(player) or not is_instance_valid(enemy):
		return false

	var query := PhysicsRayQueryParameters2D.create(enemy.global_position, player.global_position)
	query.collision_mask = COVER_QUERY_MASK
	query.exclude = [enemy.get_rid(), player.get_rid()]

	var hit := get_world_2d().direct_space_state.intersect_ray(query)
	return hit.is_empty()


func _projectile_hits_wall(projectile: Projectile) -> bool:
	var projectile_position := projectile.global_position
	var previous_position := projectile.get_previous_position()
	var projectile_radius := float(projectile.radius)

	for cover_rect in cover_rects:
		var expanded := cover_rect.grow(projectile_radius)
		if expanded.has_point(projectile_position) or expanded.has_point(previous_position):
			return true

	return false


func _spawn_buff(spawn_position: Vector2, buff_type: StringName = &"") -> void:
	if not is_instance_valid(entity_root):
		return

	var buff: Buff = BUFF_SCRIPT.new()
	buff.global_position = spawn_position

	if buff_type != &"":
		buff.setup(buff_type)
	else:
		var can_drop_ammo := is_instance_valid(player) and bool(player.get_weapon_config().get("uses_ammo", true))
		match rng.randi_range(0, 4 if can_drop_ammo else 3):
			0:
				buff.setup(&"speed")
			1:
				buff.setup(&"fire_rate")
			2:
				buff.setup(&"shield")
			3:
				if rng.randf() < 0.5:
					buff.setup(&"health")
				else:
					buff.setup(&"armor")
			4:
				buff.setup(&"ammo")

	entity_root.add_child(buff)
	buffs.append(buff)


func _handle_buff_pickups() -> void:
	if not is_instance_valid(player):
		return

	for buff in buffs:
		if not is_instance_valid(buff):
			continue

		var hit_distance := player.get_collision_radius() + buff.get_collision_radius()
		if player.global_position.distance_to(buff.global_position) <= hit_distance:
			if buff.buff_type == &"health":
				var healed_amount := player.apply_heal(HEAL_AMOUNT)
				if healed_amount > 0.0:
					_show_banner("ХИЛКА: +%d HP" % int(round(healed_amount)), 0.9)
			elif buff.buff_type == &"ammo":
				var ammo_gained := player.add_ammo(player.get_ammo_pickup_amount())
				if ammo_gained > 0:
					_show_banner("ПАТРОНЫ: +%d" % ammo_gained, 0.9)
				else:
					player.apply_buff(&"armor")
					_show_banner("ПАТРОНЫ -> БРОНЯ", 0.9)
			elif buff.buff_type == &"armor":
				player.apply_buff(buff.buff_type)
				_show_banner("БРОНЯ: +35" , 0.9)
			else:
				player.apply_buff(buff.buff_type)
				_show_banner(_buff_pickup_label(buff.buff_type), 0.9)
			_spawn_impact(buff.global_position, Color(0.4, 1.0, 0.5), 18.0)
			_push_screen_shake(2.5)
			if is_instance_valid(audio_manager):
				audio_manager.play_pickup()
			buff.queue_free()


func _buff_pickup_label(buff_type: StringName) -> String:
	match buff_type:
		&"speed":
			return "БАФФ: СКОРОСТЬ"
		&"fire_rate":
			return "БАФФ: СКОРОСТРЕЛЬНОСТЬ"
		&"shield":
			return "БАФФ: ЩИТ +50"
		&"health":
			return "БАФФ: ХИЛКА +35"
		&"armor":
			return "БАФФ: БРОНЯ +35"
		&"ammo":
			return "БАФФ: ПАТРОНЫ"
	return "БАФФ"


func _spawn_impact(at_position: Vector2, color: Color, radius: float) -> void:
	if not is_instance_valid(effects_root):
		return

	var impact: ImpactEffect = IMPACT_EFFECT_SCRIPT.new()
	impact.setup(at_position, color, radius)
	effects_root.add_child(impact)


func _compact_runtime_arrays() -> void:
	var alive_enemies: Array[Enemy] = []
	for enemy in enemies:
		if is_instance_valid(enemy):
			alive_enemies.append(enemy)
	enemies = alive_enemies

	var alive_projectiles: Array[Projectile] = []
	for projectile in projectiles:
		if is_instance_valid(projectile):
			alive_projectiles.append(projectile)
	projectiles = alive_projectiles

	var alive_buffs: Array[Buff] = []
	for buff in buffs:
		if is_instance_valid(buff):
			alive_buffs.append(buff)
	buffs = alive_buffs


func _on_player_health_changed(current_hp: float, max_hp: float, shield_hp_value: float) -> void:
	hp_bar.max_value = max_hp
	hp_bar.value = current_hp
	hp_label.text = "HP: %d / %d" % [int(round(current_hp)), int(round(max_hp))]
	shield_label.text = "Щит: %d" % int(round(shield_hp_value))

	var hp_ratio := current_hp / maxf(max_hp, 1.0)
	if hp_ratio <= 0.3:
		hp_bar.modulate = Color(1.0, 0.45, 0.4)
	elif hp_ratio <= 0.6:
		hp_bar.modulate = Color(1.0, 0.9, 0.45)
	else:
		hp_bar.modulate = Color(0.75, 1.0, 0.75)


func _on_player_died() -> void:
	_finish_run(false)


func _finish_run(victory: bool) -> void:
	if run_finished:
		return

	_set_pause_enabled(false)
	weapon_choice_active = false
	pending_weapon_choice_victory = false
	if weapon_choice_root:
		weapon_choice_root.visible = false
	run_finished = true
	run_victory = victory
	state = GameState.FINISHED

	hud_root.visible = false
	end_root.visible = true

	if run_victory:
		end_title_label.text = "ВЫЖИЛ!"
		if is_instance_valid(audio_manager):
			audio_manager.play_win()
	else:
		end_title_label.text = "ПОРАЖЕНИЕ"
		if is_instance_valid(audio_manager):
			audio_manager.play_lose()

	if score > best_score:
		best_score = score
		_save_persistent_data()

	_reset_combo()
	_refresh_best_score_labels()
	end_score_label.text = "Очки: %d" % score


func _show_banner(text: String, duration: float) -> void:
	wave_banner.text = text
	wave_banner.visible = true
	wave_banner.modulate = Color(1.0, 1.0, 1.0, 1.0)
	wave_banner_left = duration


func _update_hud() -> void:
	score_label.text = "Очки: %d" % score
	wave_label.text = "Волна: %d/%d" % [current_wave, MAX_WAVES]
	enemies_label.text = "Враги: %d" % enemies.size()

	if is_instance_valid(player):
		buff_label.text = player.get_buff_status_text()
		weapon_label.text = player.get_weapon_status_text()
		armor_label.text = player.get_armor_status_text()
		var adrenaline_text := player.get_adrenaline_status_text()
		adrenaline_label.text = adrenaline_text if adrenaline_text != "" else "Адреналин: нет"
		dash_label.text = player.get_dash_status_text()

	if combo_count > 1 and combo_time_left > 0.0:
		combo_label.text = "Комбо: x%.2f (%.1fs)" % [combo_multiplier, combo_time_left]
	else:
		combo_label.text = "Комбо: x1.00"


func _get_nearest_enemy_position() -> Variant:
	if not is_instance_valid(player):
		return null

	var nearest_enemy: Enemy = null
	var nearest_distance_sq := INF

	for enemy in enemies:
		if not is_instance_valid(enemy):
			continue
		var distance_sq := player.global_position.distance_squared_to(enemy.global_position)
		if distance_sq < nearest_distance_sq:
			nearest_distance_sq = distance_sq
			nearest_enemy = enemy

	if nearest_enemy == null:
		return null
	return nearest_enemy.global_position


func _configure_input_map() -> void:
	var input_setup := {
		"move_left": [KEY_A, KEY_LEFT],
		"move_right": [KEY_D, KEY_RIGHT],
		"move_up": [KEY_W, KEY_UP],
		"move_down": [KEY_S, KEY_DOWN]
	}

	for action_name in input_setup.keys():
		if not InputMap.has_action(action_name):
			InputMap.add_action(action_name)

		for event in InputMap.action_get_events(action_name):
			InputMap.action_erase_event(action_name, event)

		for keycode in input_setup[action_name]:
			var key_event := InputEventKey.new()
			key_event.physical_keycode = keycode
			InputMap.action_add_event(action_name, key_event)

	if not InputMap.has_action("dash"):
		InputMap.add_action("dash")

	for event in InputMap.action_get_events("dash"):
		InputMap.action_erase_event("dash", event)

	for keycode in [KEY_SPACE, KEY_SHIFT]:
		var dash_key_event := InputEventKey.new()
		dash_key_event.physical_keycode = keycode
		InputMap.action_add_event("dash", dash_key_event)

	if not InputMap.has_action("reload"):
		InputMap.add_action("reload")

	for event in InputMap.action_get_events("reload"):
		InputMap.action_erase_event("reload", event)

	var reload_key_event := InputEventKey.new()
	reload_key_event.physical_keycode = KEY_R
	InputMap.action_add_event("reload", reload_key_event)


func _input(event: InputEvent) -> void:
	if not (event is InputEventKey):
		return

	var key_event := event as InputEventKey
	if not key_event.pressed or key_event.echo:
		return

	if weapon_choice_active:
		return

	if key_event.physical_keycode != KEY_ESCAPE:
		return

	if state == GameState.PLAYING and not run_finished:
		_set_pause_enabled(not is_paused)
	elif state == GameState.FINISHED:
		_show_menu()


func _set_pause_enabled(enabled: bool) -> void:
	var can_pause := state == GameState.PLAYING and not run_finished
	is_paused = enabled and can_pause
	get_tree().paused = is_paused

	if pause_root:
		pause_root.visible = is_paused


func _on_pause_menu_pressed() -> void:
	_set_pause_enabled(false)
	_show_menu()


func _load_persistent_data() -> void:
	var config := ConfigFile.new()
	if config.load(SAVE_PATH) == OK:
		best_score = int(config.get_value("stats", "best_score", 0))
	else:
		best_score = 0


func _save_persistent_data() -> void:
	var config := ConfigFile.new()
	config.set_value("stats", "best_score", best_score)
	config.save(SAVE_PATH)


func _refresh_best_score_labels() -> void:
	if menu_best_label:
		menu_best_label.text = "Рекорд: %d" % best_score

	if end_best_label:
		end_best_label.text = "Рекорд: %d" % best_score


func _open_weapon_choice() -> void:
	if weapon_choice_active or not is_instance_valid(player):
		return

	weapon_choice_options = _build_weapon_choice_options()
	weapon_choice_active = true
	pending_weapon_choice_victory = current_wave >= MAX_WAVES
	state = GameState.WEAPON_CHOICE
	get_tree().paused = true

	if weapon_choice_root:
		weapon_choice_root.visible = true

	var box := weapon_choice_root.get_child(1).get_child(0) if weapon_choice_root and weapon_choice_root.get_child_count() > 1 else null
	if box == null:
		return

	var title := box.get_child(0) as Label
	var subtitle := box.get_child(1) as Label
	if title:
		title.text = "ВЫБОР ОРУЖИЯ"
	if subtitle:
		if is_instance_valid(player):
			subtitle.text = "Текущее: %s\n%s" % [player.get_weapon_name(), player.get_weapon_upgrade_preview_text(player.get_weapon_type())]
		else:
			subtitle.text = "Текущее оружие: Пистолет"

	for index in range(weapon_choice_options.size()):
		var button := box.get_child(index + 2) as Button
		if button:
			button.text = _weapon_choice_button_text(weapon_choice_options[index])
			button.disabled = false

	for index in range(weapon_choice_options.size(), box.get_child_count() - 2):
		var extra_button := box.get_child(index + 2) as Button
		if extra_button:
			extra_button.disabled = true


func _on_weapon_choice_pressed(weapon_type: StringName) -> void:
	if not weapon_choice_active or not is_instance_valid(player):
		return

	if player.get_weapon_type() == weapon_type:
		player.upgrade_weapon()
	else:
		player.set_weapon_type(weapon_type)
	weapon_choice_active = false
	if weapon_choice_root:
		weapon_choice_root.visible = false
	get_tree().paused = false
	state = GameState.PLAYING if not pending_weapon_choice_victory else GameState.FINISHED

	if pending_weapon_choice_victory:
		_finish_run(true)
		return

	_start_wave(current_wave + 1)


func _on_weapon_choice_button_pressed(option_index: int) -> void:
	if option_index < 0 or option_index >= weapon_choice_options.size():
		return

	_on_weapon_choice_pressed(weapon_choice_options[option_index])


func _weapon_choice_button_text(weapon_type: StringName) -> String:
	var weapon_name := _weapon_choice_label(weapon_type)
	if is_instance_valid(player) and player.get_weapon_type() == weapon_type:
		return "ОСТАВИТЬ И УЛУЧШИТЬ: %s\n%s" % [weapon_name, player.get_weapon_upgrade_preview_text(weapon_type)]

	if is_instance_valid(player):
		return "ВЗЯТЬ: %s\n%s" % [weapon_name, player.get_weapon_upgrade_preview_text(weapon_type)]

	return "ВЗЯТЬ: %s" % weapon_name


func _weapon_choice_label(weapon_type: StringName) -> String:
	match weapon_type:
		&"minigun":
			return "Пулемет - безумный темп, огромный магазин"
		&"shotgun":
			return "Дробовик - 5 пуль, мощный разлет"
		&"flamethrower":
			return "Огнемет - почти непрерывная струя"
		&"sniper":
			return "Снайперка - one shot для обычных мобов, пули проходят сквозь"
		&"axe":
			return "Топор - ближний бой, но даёт броню"
		_:
			return "Пистолет - стабильный одиночный огонь"


func _build_weapon_choice_options() -> Array[StringName]:
	if not is_instance_valid(player):
		return [&"pistol", &"shotgun", &"flamethrower"]

	var current_weapon := player.get_weapon_type()
	var all_weapons := player.get_all_weapon_types()
	var pool: Array[StringName] = []
	var options: Array[StringName] = [current_weapon]

	for weapon_type in all_weapons:
		if weapon_type != current_weapon:
			pool.append(weapon_type)

	while options.size() < 3 and not pool.is_empty():
		var index := rng.randi_range(0, pool.size() - 1)
		options.append(pool[index])
		pool.remove_at(index)

	return options


func _calculate_combo_score(base_score: int) -> int:
	if combo_time_left > 0.0:
		combo_count += 1
	else:
		combo_count = 1

	combo_time_left = COMBO_TIMEOUT
	combo_multiplier = minf(MAX_COMBO_MULTIPLIER, 1.0 + float(combo_count - 1) * COMBO_STEP)
	return int(round(float(base_score) * combo_multiplier))


func _reset_combo() -> void:
	combo_count = 0
	combo_multiplier = 1.0
	combo_time_left = 0.0


func _push_screen_shake(amount: float) -> void:
	screen_shake = minf(MAX_SHAKE, screen_shake + amount)


func _update_screen_shake(delta: float) -> void:
	if not is_instance_valid(gameplay_camera):
		return

	if screen_shake <= 0.0:
		if gameplay_camera.offset != Vector2.ZERO:
			gameplay_camera.offset = Vector2.ZERO
		return

	gameplay_camera.offset = Vector2(
		rng.randf_range(-screen_shake, screen_shake),
		rng.randf_range(-screen_shake, screen_shake)
	)
	screen_shake = maxf(0.0, screen_shake - SHAKE_DECAY * delta)

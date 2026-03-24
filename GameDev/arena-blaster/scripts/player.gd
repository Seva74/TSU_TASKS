extends CharacterBody2D
class_name Player

signal shoot_requested(origin: Vector2, direction: Vector2, damage: float, weapon_type: StringName)
signal health_changed(current_hp: float, max_hp: float, shield_hp: float)
signal died

const BASE_HP := 100.0
const BASE_SPEED := 300.0
const RADIUS := 18.0
const BUFF_DURATION := 5.0
const DASH_SPEED := 980.0
const DASH_DURATION := 0.15
const DASH_COOLDOWN := 1.2

const WEAPON_PISTOL := &"pistol"
const WEAPON_MINIGUN := &"minigun"
const WEAPON_SHOTGUN := &"shotgun"
const WEAPON_FLAMETHROWER := &"flamethrower"
const WEAPON_SNIPER := &"sniper"
const WEAPON_AXE := &"axe"

const WEAPON_CONFIGS := {
	WEAPON_PISTOL: {
		"label": "Пистолет",
		"damage": 10.0,
		"fire_interval": 0.5,
		"pellets": 1,
		"spread": 0.0,
		"projectile_speed": 940.0,
		"projectile_lifetime": 1.7,
		"uses_ammo": true,
		"magazine_size": 12,
		"reserve_ammo": 48,
		"reload_time": 1.15,
		"armor_on_equip": 0,
		"texture": preload("res://assets/kenney_topdown_shooter/PNG/Man Blue/manBlue_gun.png")
	},
	WEAPON_MINIGUN: {
		"label": "Пулемет",
		"damage": 6.0,
		"fire_interval": 0.065,
		"pellets": 1,
		"spread": 0.0,
		"projectile_speed": 1180.0,
		"projectile_lifetime": 1.45,
		"uses_ammo": true,
		"magazine_size": 60,
		"reserve_ammo": 180,
		"reload_time": 2.6,
		"armor_on_equip": 0,
		"texture": preload("res://assets/kenney_topdown_shooter/PNG/Man Blue/manBlue_machine.png")
	},
	WEAPON_SHOTGUN: {
		"label": "Дробовик",
		"damage": 5.0,
		"fire_interval": 0.82,
		"pellets": 7,
		"spread": 0.48,
		"projectile_speed": 860.0,
		"projectile_lifetime": 1.12,
		"uses_ammo": true,
		"magazine_size": 6,
		"reserve_ammo": 18,
		"reload_time": 1.75,
		"armor_on_equip": 0,
		"texture": preload("res://assets/kenney_topdown_shooter/PNG/Man Blue/manBlue_machine.png")
	},
	WEAPON_FLAMETHROWER: {
		"label": "Огнемет",
		"damage": 2.0,
		"fire_interval": 0.04,
		"pellets": 2,
		"spread": 0.22,
		"projectile_speed": 700.0,
		"projectile_lifetime": 0.38,
		"uses_ammo": true,
		"magazine_size": 24,
		"reserve_ammo": 72,
		"reload_time": 2.4,
		"armor_on_equip": 0,
		"texture": preload("res://assets/kenney_topdown_shooter/PNG/Man Blue/manBlue_silencer.png")
	},
	WEAPON_SNIPER: {
		"label": "Снайперка",
		"damage": 120.0,
		"fire_interval": 1.15,
		"pellets": 1,
		"spread": 0.0,
		"projectile_speed": 1700.0,
		"projectile_lifetime": 2.2,
		"uses_ammo": true,
		"magazine_size": 3,
		"reserve_ammo": 9,
		"reload_time": 1.8,
		"pierce_enemies": 4,
		"armor_on_equip": 0,
		"texture": preload("res://assets/kenney_topdown_shooter/PNG/Man Blue/manBlue_gun.png")
	},
	WEAPON_AXE: {
		"label": "Топор",
		"damage": 52.0,
		"fire_interval": 0.78,
		"pellets": 1,
		"spread": 0.0,
		"melee_range": 94.0,
		"melee_arc": 1.3,
		"uses_ammo": false,
		"magazine_size": 0,
		"reserve_ammo": 0,
		"reload_time": 0.0,
		"armor_on_equip": 80,
		"texture": preload("res://assets/kenney_topdown_shooter/PNG/Man Blue/manBlue_hold.png")
	}
}

var max_hp := BASE_HP
var hp := BASE_HP
var shield_hp := 0.0
var temporary_shield_hp := 0.0

var speed_multiplier := 1.0
var fire_rate_multiplier := 1.0
var weapon_type: StringName = WEAPON_PISTOL
var ammo_in_mag := 0
var reserve_ammo := 0
var is_reloading := false
var reload_time_left := 0.0
var armor_hp := 0.0
var weapon_rank := 1
var adrenaline_left := 0.0
var adrenaline_speed_multiplier := 1.0
var adrenaline_fire_rate_multiplier := 1.0

var speed_buff_left := 0.0
var fire_rate_buff_left := 0.0
var shield_buff_left := 0.0

var shoot_cooldown := 0.0
var arena_rect := Rect2(Vector2.ZERO, Vector2(1920.0, 1080.0))
var last_move_direction := Vector2.RIGHT
var dash_direction := Vector2.RIGHT
var dash_time_left := 0.0
var dash_cooldown_left := 0.0

var sprite: Sprite2D


func _ready() -> void:
	_build_visuals()
	_apply_weapon_visuals()
	_reset_weapon_ammo()
	health_changed.emit(hp, max_hp, shield_hp)


func _physics_process(delta: float) -> void:
	_update_buffs(delta)
	_update_adrenaline(delta)
	_update_dash_state(delta)
	_process_movement()
	_process_weapon_fire(delta)


func _update_dash_state(delta: float) -> void:
	dash_time_left = maxf(0.0, dash_time_left - delta)
	dash_cooldown_left = maxf(0.0, dash_cooldown_left - delta)


func _build_visuals() -> void:
	sprite = Sprite2D.new()
	add_child(sprite)

	var hitbox := CollisionShape2D.new()
	var shape := CircleShape2D.new()
	shape.radius = RADIUS
	hitbox.shape = shape
	add_child(hitbox)


func _process_movement() -> void:
	var input_vector := Vector2(
		Input.get_action_strength("move_right") - Input.get_action_strength("move_left"),
		Input.get_action_strength("move_down") - Input.get_action_strength("move_up")
	)

	if input_vector.length() > 1.0:
		input_vector = input_vector.normalized()

	if input_vector.length_squared() > 0.0001:
		last_move_direction = input_vector.normalized()

	if dash_time_left <= 0.0 and dash_cooldown_left <= 0.0 and Input.is_action_just_pressed("dash"):
		dash_direction = last_move_direction
		dash_time_left = DASH_DURATION
		dash_cooldown_left = DASH_COOLDOWN

	if dash_time_left > 0.0:
		velocity = dash_direction * DASH_SPEED
	else:
		velocity = input_vector * BASE_SPEED * speed_multiplier * adrenaline_speed_multiplier

	move_and_slide()

	global_position.x = clampf(global_position.x, arena_rect.position.x + RADIUS, arena_rect.end.x - RADIUS)
	global_position.y = clampf(global_position.y, arena_rect.position.y + RADIUS, arena_rect.end.y - RADIUS)


func _process_weapon_fire(delta: float) -> void:
	_update_reload_state(delta)
	var weapon_config: Dictionary = _get_weapon_config()
	var uses_ammo := bool(weapon_config.get("uses_ammo", true))

	if Input.is_action_just_pressed("reload"):
		if uses_ammo:
			_start_reload()

	if is_reloading:
		return

	shoot_cooldown = maxf(shoot_cooldown - delta, 0.0)
	if shoot_cooldown > 0.0:
		return

	if not Input.is_mouse_button_pressed(MOUSE_BUTTON_LEFT):
		return

	var aim_direction := _get_mouse_aim_direction()
	if aim_direction == Vector2.ZERO:
		return

	if uses_ammo and ammo_in_mag <= 0:
		_start_reload()
		return

	var pellets := int(weapon_config["pellets"])
	var spread := float(weapon_config["spread"])
	var damage := float(weapon_config["damage"])
	var fire_interval := float(weapon_config["fire_interval"])
	var pellet_step := 0.0

	if pellets > 1:
		pellet_step = spread / float(pellets - 1)

	for pellet_index in range(pellets):
		var angle_offset := 0.0
		if pellets > 1:
			angle_offset = -spread * 0.5 + pellet_step * float(pellet_index)

		var fire_direction := aim_direction.rotated(angle_offset)
		shoot_requested.emit(global_position, fire_direction, damage, weapon_type)

	if uses_ammo:
		ammo_in_mag -= 1
		if ammo_in_mag <= 0 and reserve_ammo > 0:
			_start_reload()

	shoot_cooldown = fire_interval / (fire_rate_multiplier * adrenaline_fire_rate_multiplier)


func _get_mouse_aim_direction() -> Vector2:
	var direction := get_global_mouse_position() - global_position
	if direction.length_squared() <= 0.0001:
		return Vector2.ZERO

	return direction.normalized()


func _update_buffs(delta: float) -> void:
	if speed_buff_left > 0.0:
		speed_buff_left -= delta
		if speed_buff_left <= 0.0:
			speed_multiplier = 1.0

	if fire_rate_buff_left > 0.0:
		fire_rate_buff_left -= delta
		if fire_rate_buff_left <= 0.0:
			fire_rate_multiplier = 1.0

	if shield_buff_left > 0.0:
		shield_buff_left -= delta
		if shield_buff_left <= 0.0 and temporary_shield_hp > 0.0:
			shield_hp = maxf(0.0, shield_hp - temporary_shield_hp)
			temporary_shield_hp = 0.0
			health_changed.emit(hp, max_hp, shield_hp)


func set_arena_size(size: Vector2) -> void:
	arena_rect = Rect2(Vector2.ZERO, size)


func apply_damage(amount: float) -> void:
	var incoming := amount

	if armor_hp > 0.0:
		var absorbed_armor := minf(armor_hp, incoming)
		armor_hp -= absorbed_armor
		incoming -= absorbed_armor

	if shield_hp > 0.0:
		var absorbed := minf(shield_hp, incoming)
		shield_hp -= absorbed
		incoming -= absorbed

		if temporary_shield_hp > 0.0:
			var absorbed_temp := minf(temporary_shield_hp, absorbed)
			temporary_shield_hp -= absorbed_temp

	if incoming > 0.0:
		hp = maxf(0.0, hp - incoming)

	health_changed.emit(hp, max_hp, shield_hp)
	if hp <= 0.0:
		died.emit()


func apply_buff(buff_type: StringName) -> void:
	match buff_type:
		&"speed":
			speed_multiplier = 1.5
			speed_buff_left = BUFF_DURATION
		&"fire_rate":
			fire_rate_multiplier = 2.0
			fire_rate_buff_left = BUFF_DURATION
		&"shield":
			shield_hp += 50.0
			temporary_shield_hp += 50.0
			shield_buff_left = BUFF_DURATION
		&"armor":
			armor_hp += 35.0

	health_changed.emit(hp, max_hp, shield_hp)


func apply_heal(amount: float) -> float:
	if amount <= 0.0:
		return 0.0

	var previous_hp := hp
	hp = minf(max_hp, hp + amount)
	if hp != previous_hp:
		health_changed.emit(hp, max_hp, shield_hp)
	return hp - previous_hp


func get_armor_hp() -> float:
	return armor_hp


func add_ammo(amount: int) -> int:
	if amount <= 0:
		return 0

	var weapon_config: Dictionary = _get_weapon_config()
	if not bool(weapon_config.get("uses_ammo", true)):
		return 0

	var previous_reserve := reserve_ammo
	var max_reserve := get_reserve_ammo_capacity()
	if max_reserve <= 0:
		return 0

	reserve_ammo = mini(reserve_ammo + amount, max_reserve)
	return reserve_ammo - previous_reserve


func get_reserve_ammo_capacity() -> int:
	var weapon_config: Dictionary = _get_weapon_config()
	if not bool(weapon_config.get("uses_ammo", true)):
		return 0

	return int(weapon_config.get("magazine_size", 12)) * 5


func set_weapon_type(new_weapon_type: StringName) -> void:
	if not WEAPON_CONFIGS.has(new_weapon_type):
		return

	weapon_type = new_weapon_type
	weapon_rank = 1
	_apply_weapon_visuals()
	_reset_weapon_ammo()
	armor_hp = float(_get_weapon_config().get("armor_on_equip", 0.0))


func upgrade_weapon() -> void:
	weapon_rank += 1
	_apply_weapon_visuals()
	_reset_weapon_ammo()
	armor_hp = float(_get_weapon_config().get("armor_on_equip", 0.0))


func get_weapon_type() -> StringName:
	return weapon_type


func get_weapon_name() -> String:
	return "%s +%d" % [String(_get_weapon_config()["label"]), weapon_rank - 1] if weapon_rank > 1 else String(_get_weapon_config()["label"])


func get_weapon_config() -> Dictionary:
	return _get_weapon_config().duplicate(true)


func get_weapon_status_text() -> String:
	var weapon_config: Dictionary = _get_weapon_config()
	var label := get_weapon_name()
	var fire_interval := float(weapon_config["fire_interval"])
	var pellets := int(weapon_config["pellets"])
	var damage := float(weapon_config["damage"])
	var uses_ammo := bool(weapon_config.get("uses_ammo", true))
	var ammo_text := "∞"
	var reserve_text := "∞"
	var reload_text := ""

	if uses_ammo:
		ammo_text = "%d/%d" % [ammo_in_mag, int(weapon_config["magazine_size"])]
		reserve_text = "%d" % reserve_ammo

	if is_reloading:
		reload_text = " | ПЕРЕЗАРЯДКА %.1fs" % reload_time_left

	if pellets > 1:
		return "Оружие: %s | %.1f x%d | %s + %s | %.2fs%s" % [label, damage, pellets, ammo_text, reserve_text, fire_interval / fire_rate_multiplier, reload_text]

	return "Оружие: %s | %.1f | %s + %s | %.2fs%s" % [label, damage, ammo_text, reserve_text, fire_interval / fire_rate_multiplier, reload_text]


func get_buff_status_text() -> String:
	var statuses: Array[String] = []
	if speed_buff_left > 0.0:
		statuses.append("Скорость x1.5: %.1fs" % speed_buff_left)
	if fire_rate_buff_left > 0.0:
		statuses.append("Огонь x2: %.1fs" % fire_rate_buff_left)
	if shield_buff_left > 0.0:
		statuses.append("Щит +50: %.1fs" % shield_buff_left)
	if adrenaline_left > 0.0:
		statuses.append("Адреналин x%.2f: %.1fs" % [adrenaline_fire_rate_multiplier, adrenaline_left])

	if statuses.is_empty():
		return "Баффы: нет"
	return "Баффы: " + ", ".join(statuses)


func get_armor_status_text() -> String:
	if armor_hp > 0.0:
		return "Броня: %d" % int(round(armor_hp))
	return "Броня: 0"


func apply_adrenaline(duration: float) -> void:
	if duration <= 0.0:
		return

	adrenaline_left = maxf(adrenaline_left, duration)
	adrenaline_speed_multiplier = 1.12
	adrenaline_fire_rate_multiplier = 1.25


func get_adrenaline_status_text() -> String:
	if adrenaline_left > 0.0:
		return "Адреналин: %.1fs" % adrenaline_left
	return ""


func get_collision_radius() -> float:
	return RADIUS


func is_dash_invulnerable() -> bool:
	return dash_time_left > 0.0


func get_dash_status_text() -> String:
	if dash_time_left > 0.0:
		return "Рывок: АКТИВЕН"
	if dash_cooldown_left > 0.0:
		return "Рывок: %.1fs" % dash_cooldown_left
	return "Рывок: READY"


func get_all_weapon_types() -> Array[StringName]:
	return [WEAPON_PISTOL, WEAPON_MINIGUN, WEAPON_SHOTGUN, WEAPON_FLAMETHROWER, WEAPON_SNIPER, WEAPON_AXE]


func _update_adrenaline(delta: float) -> void:
	if adrenaline_left <= 0.0:
		adrenaline_speed_multiplier = 1.0
		adrenaline_fire_rate_multiplier = 1.0
		return

	adrenaline_left = maxf(0.0, adrenaline_left - delta)
	if adrenaline_left <= 0.0:
		adrenaline_speed_multiplier = 1.0
		adrenaline_fire_rate_multiplier = 1.0


func _get_weapon_config() -> Dictionary:
	var base_config: Dictionary = WEAPON_CONFIGS.get(weapon_type, WEAPON_CONFIGS[WEAPON_PISTOL]).duplicate(true)
	_apply_weapon_rank_modifiers(base_config)
	return base_config


func _apply_weapon_visuals() -> void:
	if not sprite:
		return

	var weapon_config: Dictionary = _get_weapon_config()
	sprite.texture = weapon_config["texture"]


func _reset_weapon_ammo() -> void:
	var weapon_config: Dictionary = _get_weapon_config()
	if bool(weapon_config.get("uses_ammo", true)):
		ammo_in_mag = int(weapon_config.get("magazine_size", 12))
		reserve_ammo = int(weapon_config.get("reserve_ammo", 0))
	else:
		ammo_in_mag = 1
		reserve_ammo = 0
	is_reloading = false
	reload_time_left = 0.0


func _start_reload() -> void:
	if is_reloading:
		return

	var weapon_config := _get_weapon_config()
	if not bool(weapon_config.get("uses_ammo", true)):
		return
	var magazine_size := int(weapon_config.get("magazine_size", 12))
	if ammo_in_mag >= magazine_size:
		return
	if reserve_ammo <= 0:
		return

	is_reloading = true
	reload_time_left = float(weapon_config.get("reload_time", 1.0))


func _update_reload_state(delta: float) -> void:
	if not is_reloading:
		return

	if reserve_ammo <= 0:
		is_reloading = false
		reload_time_left = 0.0
		return

	reload_time_left = maxf(0.0, reload_time_left - delta)
	if reload_time_left > 0.0:
		return

	var weapon_config := _get_weapon_config()
	if not bool(weapon_config.get("uses_ammo", true)):
		is_reloading = false
		reload_time_left = 0.0
		return
	var magazine_size := int(weapon_config.get("magazine_size", 12))
	var missing := magazine_size - ammo_in_mag
	var reloaded := minf(float(missing), float(reserve_ammo))
	ammo_in_mag += int(reloaded)
	reserve_ammo -= int(reloaded)
	ammo_in_mag = clampi(ammo_in_mag, 0, magazine_size)
	reserve_ammo = maxi(reserve_ammo, 0)
	is_reloading = false
	reload_time_left = 0.0


func _apply_weapon_rank_modifiers(config: Dictionary) -> void:
	var rank_bonus: int = weapon_rank - 1
	if rank_bonus <= 0:
		return

	match weapon_type:
		WEAPON_PISTOL:
			config["damage"] = float(config["damage"]) + float(rank_bonus) * 2.0
			config["fire_interval"] = maxf(0.18, float(config["fire_interval"]) * pow(0.93, float(rank_bonus)))
			config["magazine_size"] = int(config["magazine_size"]) + rank_bonus * 3
			config["reserve_ammo"] = int(config["reserve_ammo"]) + rank_bonus * 10
		WEAPON_MINIGUN:
			config["damage"] = float(config["damage"]) + float(rank_bonus) * 1.0
			config["fire_interval"] = maxf(0.028, float(config["fire_interval"]) * pow(0.9, float(rank_bonus)))
			config["magazine_size"] = int(config["magazine_size"]) + rank_bonus * 10
			config["reserve_ammo"] = int(config["reserve_ammo"]) + rank_bonus * 30
		WEAPON_SHOTGUN:
			config["damage"] = float(config["damage"]) + float(rank_bonus) * 1.3
			config["pellets"] = int(config["pellets"]) + min(rank_bonus, 2)
			config["spread"] = float(config["spread"]) + float(rank_bonus) * 0.03
			config["magazine_size"] = int(config["magazine_size"]) + rank_bonus
			config["reserve_ammo"] = int(config["reserve_ammo"]) + rank_bonus * 4
		WEAPON_FLAMETHROWER:
			config["damage"] = float(config["damage"]) + float(rank_bonus) * 0.6
			config["fire_interval"] = maxf(0.02, float(config["fire_interval"]) * pow(0.92, float(rank_bonus)))
			config["magazine_size"] = int(config["magazine_size"]) + rank_bonus * 8
			config["reserve_ammo"] = int(config["reserve_ammo"]) + rank_bonus * 20
		WEAPON_SNIPER:
			config["damage"] = float(config["damage"]) + float(rank_bonus) * 40.0
			config["fire_interval"] = maxf(0.4, float(config["fire_interval"]) * pow(0.9, float(rank_bonus)))
			config["projectile_speed"] = float(config["projectile_speed"]) + float(rank_bonus) * 120.0
			config["magazine_size"] = int(config["magazine_size"]) + max(0, rank_bonus - 1)
			config["reserve_ammo"] = int(config["reserve_ammo"]) + rank_bonus * 3
		WEAPON_AXE:
			config["damage"] = float(config["damage"]) + float(rank_bonus) * 18.0
			config["armor_on_equip"] = float(config["armor_on_equip"]) + float(rank_bonus) * 20.0
			config["melee_range"] = float(config["melee_range"]) + float(rank_bonus) * 8.0
			config["fire_interval"] = maxf(0.38, float(config["fire_interval"]) * pow(0.9, float(rank_bonus)))

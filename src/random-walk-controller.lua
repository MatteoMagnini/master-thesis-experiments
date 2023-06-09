require("utilities")
require "libs.lua-collections"
require("boolean-networks")
local editing = require("boolean-network-editing")
local file_interface = require("file-interface")
local argos = require("argos-robot")
local pprint = require('libs.pprint')

local collect = collect
local bool_to_int = bool_to_int
local math_sqrt = math.sqrt
local math_abs = math.abs

local SEED = os.clock()*os.time() --both clock and time because if you are on a virtual machine having only one of them could cause issues
local SPEED = 15
local SENSOR_THRESHOLD = 0.1
local PRECISION = 1000
local TEST_STEPS = 2000 --script can change this value
local SENSORS_NUMBER = 8
local MIN_FORWARD_STEPS = 20
local MAX_FORWARD_STEPS = 60
local WHEELS_VALUES = {1.0,1.0}
local MIN_TURN_ANGLE = - math.pi/2
local MAX_TURN_ANGLE = math.pi/2
local USE_DUAL_ENCODING = false -- if true: obstacles are encoded as false and false values will turn on the wheels

function random_double(minimum, maximum)
    return minimum + math.random() * (maximum - minimum);
end

local n_step = 0
local random_steps = 0
local random_steps_counter = 0
local angle_to_turn = random_double(MIN_TURN_ANGLE,MAX_TURN_ANGLE)
local steps_to_turn = 20 -- why 20? Because with ticks_per_second="10" it's the number of steps needed to turn by a specific angle as angular velocity.
local proximity_values = {}
local positions = {}
local states = {}

function init()
  math.randomseed(SEED)
  random_steps = math.random(MIN_FORWARD_STEPS,MAX_FORWARD_STEPS)
end

function collect_current_state()

  function table_concat(t1,t2)
    local t3 = {}
    for i=1,#t1 do
        t3[i] = t1[i]
    end
    for i=1,#t2 do
        t3[#t1+i] = t2[i]
    end
    return t3
  end
  
  states[n_step] = table_concat(get_binary_input(get_input()),WHEELS_VALUES)
end

function collect_current_proximity_values(values)
  proximity_values[n_step] = collect(values):mapValues(function(x) return math.floor(x * PRECISION)/PRECISION end):all()
end

function collect_robot_position()
  local x = math.floor((robot.positioning.position.x) * PRECISION) / PRECISION
  local y = math.floor((robot.positioning.position.y) * PRECISION) / PRECISION
  positions[n_step] = {x,y}
end

function get_input()
  return argos.get_proximity_values(SENSORS_NUMBER) --script can change this value
end

function get_binary_input(inputs)
  return collect(inputs):mapValues(function (input)
    if(USE_DUAL_ENCODING) then
      if input <= SENSOR_THRESHOLD then
        return 1.0
      else
        return 0.0
      end
    else
      if input > SENSOR_THRESHOLD then
        return 1.0
      else
        return 0.0
      end
    end
  end):all()
end

function move_forward()
  argos.move_robot_by_booleans(WHEELS_VALUES, SPEED)
end

function turn(angle)
  left_v = - angle * argos.get_wheels_distance() / 2
	right_v = angle * argos.get_wheels_distance() / 2
	robot.wheels.set_velocity(left_v,right_v)
end

function step()
  n_step = n_step + 1
  if n_step <= TEST_STEPS then
    -- Collect data
    random_steps_counter = random_steps_counter + 1
    collect_current_state()
    collect_robot_position()
    local inputs = get_input()
    collect_current_proximity_values(inputs)
    
    if random_steps_counter <= random_steps then
      move_forward()
    else
      if(steps_to_turn > 0) then
        turn(angle_to_turn)
        steps_to_turn = steps_to_turn - 1
      else
        random_steps = math.random(MIN_FORWARD_STEPS,MAX_FORWARD_STEPS)
        random_steps_counter = 0
        steps_to_turn = 20
        angle_to_turn = random_double(MIN_TURN_ANGLE,MAX_TURN_ANGLE)
      end
    end    
  end
end

function get_data_per_step()
  local output = ""
  for i=1, n_step do
    output = output .. one_line_serialize(states[i]) .. "; " .. one_line_serialize(proximity_values[i]) .. "; " .. one_line_serialize(positions[i]) .. "\n"
  end
  return output
end

function destroy()
  if n_step > 0 then
    print("random walk seed: " .. SEED)
    local output = get_data_per_step()
    print(output)
  end
end

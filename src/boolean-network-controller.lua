require("utilities")
require "libs.lua-collections"
require("boolean-networks")
local file_interface = require("file-interface")
local editing = require("boolean-network-editing")
local argos = require("argos-robot")
local pprint = require('libs.pprint')

local collect = collect
local my_if = my_if
local bool_to_int = bool_to_int
local math_sqrt = math.sqrt
local math_abs = math.abs

local network ---@type BooleanNetwork
local network_fitness = 0
local n_step = 0
local sensors_values = {}
local positions = {}
local network_states = {} --contains the history of network states

local SPEED = 15
local PRECISION = 1000
local SENSOR_THRESHOLD = 0.1
local NETWORK_TEST_STEPS = 500 --script can change this value
local PRINT_MIDWAY_RESULTS = true

local LOAD = true
local FILENAME = "../experiments/data/pt/nodes_20/experiment194.csv"

local USE_DUAL_ENCODING = false -- if true: obstacles are encoded as false and false values will turn on the wheels
local network_options = { ["node_count"] = 100, ["nodes_input_count"] = 3, ["bias"] = 0.79, --script edited
                          ["network_inputs_count"] = 8, ["network_outputs_count"] = 2,
                          ["self_loops"] = false, ["override_output_nodes_bias"] = true}

function init()
  math.randomseed(os.clock()*os.time()) --both clock and time because if you are on a virtual machine having only one of them could cause issues
  if LOAD then
    network = BooleanNetwork:load(load_network(FILENAME))
  else
    network = BooleanNetwork(network_options)  
  end
end

function collect_current_state()
  network_states[n_step] = collect(network.node_states):mapValues(bool_to_int):all()
end

function collect_current_sensors_values(values)
  sensors_values[n_step] = collect(values):mapValues(function(x) return math.floor(x * PRECISION)/PRECISION end):all()
end

function collect_robot_position()
  local x = math.floor((robot.positioning.position.x) * PRECISION) / PRECISION
  local y = math.floor((robot.positioning.position.y) * PRECISION) / PRECISION
  positions[n_step] = {x,y}
end

function get_input()
return argos.get_light_values(#network.input_nodes) --script can change this value
end

function get_binary_input(inputs)
  return argos.sensor_values_to_booleans(inputs, SENSOR_THRESHOLD, USE_DUAL_ENCODING)
end

function execute_network(inputs)
  network:force_input_values(inputs)
  local network_outputs = collect(network:update_and_get_outputs())
                          :mapValues(function (value) return my_if(USE_DUAL_ENCODING, not value, value) end):all()
  argos.move_robot_by_booleans(network_outputs, SPEED)
end

function step()
  n_step = n_step + 1
  
  if n_step <= NETWORK_TEST_STEPS then
    -- Collect data
    collect_current_state()
    collect_robot_position()
    local inputs = get_input()
    collect_current_sensors_values(inputs)
    local binary = get_binary_input(inputs)
    execute_network(binary)
    
  end    
end

function get_data_per_step()
  local output = ""
  for i=1, n_step do
    output = output .. one_line_serialize(network_states[i]) .. "; " .. one_line_serialize(sensors_values[i]) .. "; " .. one_line_serialize(positions[i]) .. "\n"
  end
  return output
end

function destroy()
  if n_step > 0 then
    print("network: " .. one_line_serialize(network))
    local output = get_data_per_step()
    print(output)
  end
end

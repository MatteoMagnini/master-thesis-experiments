function read_first_n_lines(filename, n)
  local f = assert(io.open(filename))
  local index = 0
  local rows = {}
  for line in f:lines() do
    index = index + 1
    rows[index] = line
    if index == n then
      f:close()
      return rows
    end
  end
end

function string:split( inSplitPattern )
  local outResults = {}
  local theStart = 1
  local theSplitStart, theSplitEnd = string.find( self, inSplitPattern, theStart )

  while theSplitStart do
      table.insert( outResults, string.sub( self, theStart, theSplitStart-1 ) )
      theStart = theSplitEnd + 1
      theSplitStart, theSplitEnd = string.find( self, inSplitPattern, theStart )
  end

  table.insert( outResults, string.sub( self, theStart ) )
  return outResults
end

function toBooleanArrays(boolean_string)
  local boolean_arrays = {}
  for left_bracket in string.gmatch(boolean_string, "([^{]+)") do
    local only_bools = left_bracket:sub(1, #left_bracket - 3)
    table.insert( boolean_arrays, toBooleanArray(only_bools))
  end
  return boolean_arrays
end

function toBooleanArray(boolean_string)
  local boolean_array = {}
  for value in string.gmatch(boolean_string, "([^, ]+)") do
    table.insert(boolean_array, value == "true")
  end
  return boolean_array
end

function toIntArrays(int_string)
  local int_arrays = {}
  for left_bracket in string.gmatch(int_string, "([^{]+)") do
    local only_ints = left_bracket:sub(1, #left_bracket - 3)
    table.insert( int_arrays, toIntArray(only_ints))
  end
  return int_arrays
end

function toIntArray(int_string)
  local int_array = {}
  for value in string.gmatch(int_string, "([^, ]+)") do
    table.insert(int_array, tonumber(value))
  end
  return int_array
end

function toIntArrayToBooleanArray(int_string)
  local boolean_array = {}
  local only_ints = int_string:sub(2, #int_string - 1)
  for value in string.gmatch(only_ints, "([^, ]+)") do
    table.insert(boolean_array, tonumber(value) == 1)
  end
  return boolean_array
end

function load_network(filename)
  local rows = read_first_n_lines(filename,2)
  local first_line, second_line = rows[1], rows[2]
  local boolean_functions = toBooleanArrays(first_line:split("boolean_functions = { ")[2]:split("},connection_matrix =")[1])
  local connection_matrix = toIntArrays(first_line:split("connection_matrix = { ")[2]:split("},input_nodes =")[1])
  local state = toIntArrayToBooleanArray(second_line:split(";")[1])
  local input = toIntArray(first_line:split("input_nodes = { ")[2]:split("},node_states =")[1])
  local output = toIntArray(first_line:split("output_nodes = { ")[2]:split("},overridden_output_functions =")[1])
  local overridden_output = toBooleanArrays(first_line:split("overridden_output_functions = { ")[2]:split("}}")[1])
  local network = {
    ['boolean_functions'] = boolean_functions,
    ['connection_matrix'] = connection_matrix,
    ["state"] = state,
    ["input"] = input,
    ["output"] = output,
    ["overridden_output"] = overridden_output
  }
  return network
end
#!/usr/bin/env python
from operator import itemgetter
import simplejson as json
import sqlite3
import pandas as pd

namespace = "/"

display_names = {
	"orientation_E_W": "East/West",
	"orientation_N_S": "North/South",
	"school": "a school",
	"childcare": "childcare facilities",
	"hawker_center": "a hawker centre",
	"supermarket": "a supermarket",
	"malls": "malls",
	"interchange_mrt": "an MRT",
}

def getDisplayName(key):
	return display_names.get(key, key)

def rreplace(s, old, new, occurrence):
	li = s.rsplit(old, occurrence)
	return new.join(li)

def processFinal(final, dbname, socket, sid):
	if not final:
		return
	final = json.loads(final)

	# # # override for testing
	# final["interchange_mrt"] = 0.7
	# final["hawker_center"] = 0.65
	# final["school"] = 0.65
	# final["supermarket"] = 0.4
	# final["orientation_E_W"] = 0.3
	# final["orientation_N_S"] = 0.2
	# final["location_area"] = "Sengkang"
	# final["max_price"] = 300000

	def getValue(key):
		return final.get(key, 0)
	def filterTuples(arr, excluded_min):
		return [(a,b) for a,b in arr if b > excluded_min]
	def sortTuples(arr, excludeZero=False):
		arr = [(i, getValue(i)) for i in arr]
		if excludeZero:
			arr = filterTuples(arr, 0)
		# arr.sort(key=itemgetter(0))
		# arr.sort(key=itemgetter(1), reverse=True)
		arr.sort(key=itemgetter(1, 0), reverse=True)
		return arr

	district = "location_area"
	budget = "max_price"
	level_type = "high_level"
	no_west_sun = "no_west_sun"
	room_types = ["2-room", "3-room", "4-room", "5-room", "3-Gen"]
	orientation = ["orientation_N_S", "orientation_E_W"]
	amenities = ["school", "childcare", "hawker_center", "supermarket", "malls", "interchange_mrt"]

	district = final.get(district)
	if district == "noPreferences":
		district = None
	budget = getValue(budget)
	level_type = getValue(level_type)
	no_west_sun = getValue(no_west_sun)
	room_types = sortTuples(room_types)
	orientation = sortTuples(orientation)
	amenities = sortTuples(amenities, True)
	filteredAmenities = filterTuples(amenities, 0.5)

	if level_type > 0.6666:
		level_type = "High"
	elif level_type > 0.3333:
		level_type = "Medium"
	else:
		level_type = "Low"
	room_type, room_type2, room_type3, room_type4, room_type5 = [room_types[i][0] for i in xrange(5)]

	swap_misc_order = False
	if orientation[0][1] == orientation[1][1]:
		orientation_cf = 0
		orientation = None
	else:
		orientation_cf = orientation[0][1]
		orientation = orientation[0][0]
	no_west_sun_pref = False
	if no_west_sun > 0:
		no_west_sun_pref = True
		if orientation and (no_west_sun > orientation_cf):
			swap_misc_order = True

	proximity_text = ", ".join(getDisplayName(a) for a,b in filteredAmenities)
	if proximity_text:
		proximity_text = " near " + rreplace(proximity_text, ",", " and ", 1)
	message = """<b>Our recommendations</b>:
		We recommend you a {level_type} level {room_type} flat{orientation}{proximity}.
		Here are the closest available units that fit your budget of ${budget:,}""".format(
		level_type=level_type.lower(),
		room_type=room_type,
		orientation=" facing {}".format(getDisplayName(orientation)) if orientation else "",
		proximity=proximity_text,
		budget=int(budget),
	)
	socket.emit('alert', {'data': message}, namespace=namespace, room=sid)

	order_queries = []
	if district:
		order_queries.append("""(CASE WHEN location_area = :district THEN 1 ELSE 0 END) DESC""")
	order_queries.append("""(CASE
		WHEN room_type = :room_type THEN 1
		WHEN room_type = :room_type2 THEN 2
		WHEN room_type = :room_type3 THEN 3
		WHEN room_type = :room_type4 THEN 4
		WHEN room_type = :room_type5 THEN 5
		ELSE 6 END) ASC""")
	order_queries.append("""(CASE WHEN level_type = :level_type THEN 1 ELSE 0 END) DESC""")
	if orientation:
		order_queries.append("""(CASE WHEN unit_direction = :orientation THEN 1 ELSE 0 END) DESC""")
	if no_west_sun_pref:
		order_queries.insert(-1 if swap_misc_order else len(order_queries), """(CASE WHEN west_sun = 'No' THEN 1 ELSE 0 END) DESC""")
	if district:
		prox_order_query = ", ".join("micro_{} ASC".format(a) for a,b in amenities)
	else:
		prox_order_query = ", ".join("macro_{a} ASC, micro_{a} ASC".format(a=a) for a,b in amenities)
	order_queries.append(prox_order_query)
	order_queries.append("""level DESC""")
	query = """
		SELECT project, address, block, level, unit, price, room_type, unit_direction, floor_area, completion_date
		FROM {dbname}
		WHERE booked = 0
		AND price <= :budget
		{ordering}
		LIMIT 3;
	""".format(
		dbname=dbname,
		ordering="ORDER BY " + ", ".join(order_queries),
	)
	params = {
		"budget": budget,
		"district": district,
		"room_type": room_type,
		"room_type2": room_type2,
		"room_type3": room_type3,
		"room_type4": room_type4,
		"room_type5": room_type5,
		"level_type": level_type,
		"orientation": "N-S" if orientation == "orientation_N_S" else "E-W",
	}
	print query
	print params
	socket.emit('debug', query, namespace=namespace, room=sid)
	socket.emit('debug', params, namespace=namespace, room=sid)

	# conn = sqlite3.connect(dbname+'.db')
	conn = sqlite3.connect('housing.db')
	df = pd.read_sql_query(query, conn, params=params)
	print df
	result = df.to_json(orient='split')
	result = json.loads(result)
	result["table_name"] = "resultTable"
	socket.emit('show_table', result, namespace=namespace, room=sid)

	query2 = """
		SELECT level, unit, booked, price
		FROM {dbname}
		WHERE project = :project
		AND block = :block
		AND room_type = :room_type;
	""".format(
		dbname=dbname,
	)
	projectD = df['project'].iloc[0]
	blockD = df['block'].iloc[0]
	room_typeD = df['room_type'].iloc[0]
	params2 = {
		"project": projectD,
		"block": blockD,
		"room_type": room_typeD,
	}
	print query2
	print params2
	socket.emit('debug', query2, namespace=namespace, room=sid)
	socket.emit('debug', params2, namespace=namespace, room=sid)

	df2 = pd.read_sql_query(query2, conn, params=params2)
	# df2 = df2.pivot(index='level', columns='unit', values='booked')
	df2["render"] = df2.apply(lambda row: row["price"] * (-1 if row["booked"] == 1 else 1), axis=1)
	df2 = df2 \
		.pivot_table(values='render', index='level', columns='unit', fill_value=-1) \
		.sort_index(axis=0, ascending=False) \
		.sort_index(axis=1, ascending=True) \
		.reset_index() \
		.rename(columns={"level": "Level / Unit"})
	print df2
	result2 = df2.to_json(orient='split')
	result2 = json.loads(result2)
	result2["table_name"] = "visTable"
	header = """Alternatives which may interest you (based on top result)
		{} Units for Block {} in Project {}:
		<span class="unitAvailable">Available</span> <span class="unitBooked">Booked</span>""".format(room_typeD, blockD, projectD)
	socket.emit('alert', {'data': header}, namespace=namespace, room=sid)
	socket.emit('show_table', result2, namespace=namespace, room=sid)

	socket.emit('show_reset_button', {}, namespace=namespace, room=sid)

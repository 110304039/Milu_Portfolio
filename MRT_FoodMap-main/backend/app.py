from flask import Flask, request, jsonify
from urllib.parse import quote_plus
# from flask_sqlalchemy import SQLAlchemy,text, or_, and_
from sqlalchemy.sql.expression import between
from flask_cors import CORS
from flask_sqlalchemy import SQLAlchemy
from sqlalchemy import text, or_, and_
from sqlalchemy import func


import random
from datetime import datetime, timedelta


password = quote_plus("xX@0180368905")
# password = quote_plus("00000")
# password = quote_plus("221003red")

app = Flask(__name__)
CORS(app, resources={r"/*": {"origins": "*"}})
# CORS(app, resources={r"/*": {"origins": "http://localhost:8080"}})
# CORS(app)
app.config['SQLALCHEMY_DATABASE_URI'] = f'mysql+pymysql://root:{password}@localhost/mrt_foodmap'  # 你的資料庫URI
# app.config['SQLALCHEMY_DATABASE_URI'] = f'mysql+pymysql://root:{password}@127.0.0.1:3307/mrt_foodmap'
app.config['DEBUG'] = True
db = SQLAlchemy(app)

@app.errorhandler(500)
def handle_500(error):
    app.logger.error(f"500 error: {error}")  # Log the error
    return jsonify({"status": "failed", "message": "Internal server error"}), 500



# ========== create tables ========== 
class Person(db.Model):
    __tablename__ = 'Person'
    PersonID = db.Column(db.String(50), primary_key=True)
    Name = db.Column(db.String(50), nullable=False)
    Password = db.Column(db.String(12), nullable=False)
    Location = db.Column(db.String(50))

class Station(db.Model):
    __tablename__ = 'Station'
    StationID = db.Column(db.String(50), primary_key=True)
    Name = db.Column(db.String(50), nullable=False)

class Store(db.Model):
    __tablename__ = 'Store'
    StoreID = db.Column(db.String(50), primary_key=True)
    Name = db.Column(db.String(50), nullable=False)
    Location = db.Column(db.String(50), nullable=False)
    Category = db.Column(db.String(50), nullable=False)
    URL = db.Column(db.String(50), nullable=False)
    Distance = db.Column(db.String(50), nullable=False)
    StationID = db.Column(db.String(50), nullable=False)

class Event(db.Model):
    __tablename__ = 'Event'
    EventID = db.Column(db.Integer, primary_key=True, autoincrement=True)
    P1_ID = db.Column(db.String(50), nullable=False)
    P2_ID = db.Column(db.String(50), nullable=False)
    Time = db.Column(db.DateTime, nullable=False)
    FoodType = db.Column(db.String(50), nullable=False)
    StationID = db.Column(db.String(50), nullable=False)

class MealPal(db.Model):
    __tablename__ = 'MealPal'
    MealPalID = db.Column(db.Integer, primary_key=True, autoincrement=True)
    P1_ID = db.Column(db.String(50), nullable=False)
    P2_ID = db.Column(db.String(50), nullable=False)

class FavoriteList(db.Model):
    __tablename__ = 'FavoriteList'
    FListID = db.Column(db.Integer, primary_key=True, autoincrement=True)
    PersonID = db.Column(db.String(50), nullable=False)
    StoreID = db.Column(db.String(50), nullable=False)

class ChatRecord(db.Model):
    __tablename__ = 'ChatRecord'
    ChatID = db.Column(db.Integer, primary_key=True, autoincrement=True)
    MealPalID = db.Column(db.Integer, nullable=False)
    P_ID = db.Column(db.String(50), nullable=False)
    Time = db.Column(db.DateTime, nullable=False)
    Content = db.Column(db.String(50), nullable=False)

class HistoryList(db.Model):
    __tablename__ = 'HistoryList'
    HListID = db.Column(db.Integer, primary_key=True, autoincrement=True)
    PersonID = db.Column(db.String(50), nullable=False)
    StoreID = db.Column(db.String(50), nullable=False)

class Comment(db.Model):
    __tablename__ = 'Comment'
    CommentID = db.Column(db.Integer, primary_key=True, autoincrement=True)
    PersonID = db.Column(db.String(50), nullable=False)
    StoreID = db.Column(db.String(50), nullable=False)
    Content = db.Column(db.String(150), nullable=False)


# login function with id, password
@app.route("/api/login/<id>/<password>", methods=["GET"])
def login(id, password):
    person = db.session.query(Person).filter_by(PersonID=id, Password=password).first()
    if person is None:
        return 'No such person', 404
    person_dict = {'PersonID': person.PersonID, 'Name': person.Name, 'Password': person.Password, 'Location': person.Location}
    return jsonify(person_dict)

# get all people
@app.route('/api/person', methods=['GET'])
def get_people():
    people = db.session.query(Person).all()
    people_list = [{'PersonID': person.PersonID, 'Name': person.Name, 'Password': person.Password, 'Location': person.Location} for person in people]
    return jsonify(people_list)

# fetch a person by id
@app.route('/api/person/<id>', methods=['GET'])
def get_person(id):
    person = db.session.get(Person, id) 
    if person is None:
        return 'No such person', 404
    person_dict = {'PersonID': person.PersonID, 'Name': person.Name, 'Password': person.Password, 'Location': person.Location}
    return jsonify(person_dict)



# get all stores
@app.route('/api/station', methods=['GET'])
def get_stations():
    stations = db.session.query(Station).all()
    stations_list = [{'StationID': station.StationID, 'Name': station.Name} for station in stations]
    return jsonify(stations_list)

# fetch a station by id (testpage)
@app.route('/api/station/<id>', methods=['GET'])
def get_station(id):
    station = db.session.get(Station, id) 
    if station is None:
        return 'No such station', 404
    station_dict = {'StationID': station.StationID, 'Name': station.Name} 
    return jsonify(station_dict)

# add station, need to add stationID and name (testpage)
@app.route('/api/addStation', methods=['POST'])
def add_station():
    response_object = {'status': 'success'}
    try:
        _json = request.json
        print(request.json)
        _stationID = _json['StationID']
        _name = _json['Name']
        if _stationID and _name and request.method == 'POST':
            sql = text("INSERT INTO Station(StationID, Name) VALUES(:stationID, :name)")
            data = {"stationID": _stationID, "name": _name}
            db.session.execute(sql, data)
            db.session.commit()
            response_object['message'] = 'Station added!'
            return jsonify(response_object)
        else:
            response_object['message'] = 'Invalid data'
            response_object['status'] = 'failed'
            return jsonify(response_object)
    except Exception as e:
        response_object['error'] = str(e)
        response_object['status'] = 'failed'
        app.logger.error(str(e))
        return jsonify(response_object)
    
# delete station, need to add stationID
@app.route('/api/deleteStation', methods=['POST'])
def delete_station():
    response_object = {'status': 'success'}
    try:
        _json = request.json
        _stationID = _json['StationID']
        if _stationID and request.method == 'POST':
            sql = text("DELETE FROM Station WHERE StationID = :stationID")
            data = {"stationID": _stationID}
            db.session.execute(sql, data)
            db.session.commit()
            response_object['message'] = 'Station deleted!'
            return jsonify(response_object)
        else:
            response_object['message'] = 'Invalid data'
            response_object['status'] = 'failed'
            return jsonify(response_object)
    except Exception as e:
        response_object['error'] = str(e)
        response_object['status'] = 'failed'
        app.logger.error(str(e))
        return jsonify(response_object)



# sign up, need to add personID, name, password, location
@app.route('/api/addPerson', methods=['POST'])
def add_person():
    response_object = {'status': 'success'}
    try:
        _json = request.json
        _PersonID = _json['PersonID']
        _Name = _json['Name']
        _Password = _json['Password']
        _Location = _json['Location']
        if _PersonID and _Name and _Password and _Location and request.method == 'POST':
            sql = text("INSERT INTO Person(PersonID, Name, Password, Location) VALUES(:PersonID, :Name, :Password, :Location)")
            data = {"PersonID": _PersonID, "Name": _Name, "Password": _Password, "Location": _Location}
            db.session.execute(sql, data)
            db.session.commit()
            response_object['message'] = 'Person added!'
            return jsonify(response_object)
        else:
            response_object['message'] = 'Invalid data'
            response_object['status'] = 'failed'
            return jsonify(response_object)
    except Exception as e:
        response_object['error'] = str(e)
        response_object['status'] = 'failed'
        app.logger.error(str(e))
        return jsonify(response_object)


# get all events
@app.route('/api/event', methods=['GET'])
def get_events():
    events = db.session.query(Event).all()
    events_list = [{'EventID': event.EventID, 'P1_ID': event.P1_ID, 'P2_ID': event.P2_ID, 'Time': event.Time, 'FoodType': event.FoodType, 'StationID':event.StationID} for event in events]
    return jsonify(events_list)

# fetch event by id
@app.route('/api/event/<id>', methods=['GET'])
def get_event(id):
    event = db.session.get(Event, id) 
    if event is None:
        return 'No such event', 404
    event_dict = {'EventID': event.EventID, 'P1_ID': event.P1_ID, 'P2_ID': event.P2_ID, 'Time': event.Time, 'FoodType': event.FoodType, 'StationID':event.StationID}
    return jsonify(event_dict)

# new event, need to add P1_ID, Time, FoodType, StationID
@app.route('/api/addEvent', methods=['POST'])
def add_event():
    response_object = {'status': 'success'}
    try:
        _json = request.json
        _P1_ID = _json['P1_ID']
        _P2_ID = "0"
        _Time = _json['Time']
        _FoodType = _json['FoodType']
        _StationID = _json['StationID']

        print(_P1_ID, _P2_ID, _Time, _FoodType, _StationID)
        if _P1_ID and _Time and _FoodType and _StationID and request.method == 'POST':
            sql = text("INSERT INTO Event(P1_ID, P2_ID, Time, FoodType, StationID) VALUES(:P1_ID, :P2_ID, :Time, :FoodType, :StationID)")
            data = {"P1_ID": _P1_ID, "P2_ID": _P2_ID, "Time": _Time, "FoodType": _FoodType, "StationID": _StationID}
            db.session.execute(sql, data)
            db.session.commit()
            response_object['message'] = 'Event added!'
        else:
            response_object['message'] = 'Invalid data'
            response_object['status'] = 'failed'

    except Exception as e:
        response_object['error'] = str(e)
        response_object['status'] = 'failed'
        app.logger.error(str(e))

    return jsonify(response_object)

# random pair, need to add P2_ID
@app.route('/api/randomPair', methods=['POST'])
def pair_event():
    try:
        data = request.get_json()
        data['Time'] = datetime.strptime(data['Time'], '%Y-%m-%d %H:%M:%S')

        query = text("""
            SELECT EventID, P1_ID, P2_ID, Time, FoodType, StationID FROM Event
            WHERE P2_ID = "0"
            AND ABS(TIMESTAMPDIFF(MINUTE, Time, :Time)) <= 60
            AND FoodType = :FoodType
            AND StationID = :StationID
            ORDER BY RAND()
            LIMIT 1
        """)

        # Execute the query
        result = db.session.execute(query, data).fetchone()
        result_dict = dict(result._asdict())  # Convert the result tuple to a dictionary
        # print(data) 
        if result:
            return jsonify({"status": "success", "event": result_dict}), 200
        else:
            return jsonify({"status": "failed", "message": "No available pair"}), 200

    except Exception as e:
        app.logger.error(str(e))
        return jsonify({"status": "failed", "message": str(e)}), 200
        # return jsonify({"status": "failed", "message": str(e)}), 500

@app.route('/api/pairSuccess', methods=['POST'])
def pair_success():
    try:
        data = request.get_json()
        print(data)
        # Change the P2_ID of the selected event to the passed P2_ID
        update_query = text("""
            UPDATE Event SET P2_ID = :P2_ID WHERE EventID = :id
        """)
        db.session.execute(update_query, {"P2_ID": data['P2_ID'], "id": data['EventID']})
        
        db.session.commit()
        return jsonify({"status": "success"}), 200

    except Exception as e:
        app.logger.error(str(e))
        return jsonify({"status": "failed", "message": str(e)}), 200
        # return jsonify({"status": "failed", "message": str(e)}), 500

# get fav list with specific personID
@app.route('/api/favorite/<person_id>', methods=['GET'])
def get_favorite(person_id):
    favorite_list = db.session.query(FavoriteList).filter_by(PersonID=person_id).all()
    
    if not favorite_list:
        return 'No favorite list found', 404
    
    favorites = []
    for favorite in favorite_list:
        store = db.session.query(Store).get(favorite.StoreID)
        if store is not None:
            favorites.append({'FListID': favorite.FListID, 'StoreID': store.StoreID, 'Name': store.Name, 'Location': store.Location, 'Distance': store.Distance, 'URL': store.URL })
    return jsonify(favorites)

# @app.route('/api/history/<person_id>', methods=['GET'])
# def get_history(person_id):
#     history_list = db.session.query(HistoryList).filter_by(PersonID=person_id).all()
    
#     if not history_list:
#         return 'No history list found', 404
    
#     histories = []
#     for history in history_list:
#         store = db.session.query(Store).get(history.StoreID)
#         if store is not None:
#             histories.append({'HListID': history.HListID, 'StoreID': store.StoreID, 'Name': store.Name, 'Location': store.Location, 'Distance': store.Distance, 'URL': store.URL })
#     return jsonify(histories)

# add Favorite
@app.route('/api/addFavorite', methods=['POST'])
def add_favorite():
    response_object = {'status': 'success'}
    try:
        _json = request.json
        # print(request.json)
        _StoreID = _json['StoreID']['data'][0]['StoreID'] #為甚麼回傳的json這麼複雜...
        _P1_ID = _json['P1_ID']
        if _StoreID and request.method == 'POST':
            sql = text("INSERT INTO FavoriteList(PersonID, StoreID) VALUES(:PersonID, :StoreID)")
            data = {"PersonID": _P1_ID, "StoreID": _StoreID}
            db.session.execute(sql, data)
            db.session.commit()
            response_object['message'] = 'Favorite added!'
            return jsonify(response_object)
        else:
            response_object['message'] = 'Invalid data'
            response_object['status'] = 'failed'
            return jsonify(response_object)
    except Exception as e:
        response_object['error'] = str(e)
        response_object['status'] = 'failed'
        app.logger.error(str(e))
        return jsonify(response_object)
    
# delete favorite
@app.route('/api/deleteFavorite', methods=['POST'])
def delete_favorite():
    response_object = {'status': 'success'}
    try:
        _json = request.json
        # print(request.json)
        _StoreID = _json['StoreID']['data'][0]['StoreID']
        _P1_ID = _json['P1_ID']
        if _StoreID and request.method == 'POST':
            sql = text("DELETE FROM FavoriteList WHERE PersonID = :PersonID and StoreID = :StoreID")
            data = {"PersonID": _P1_ID, "StoreID": _StoreID}
            db.session.execute(sql, data)
            db.session.commit()
            response_object['message'] = 'Station deleted!'
            return jsonify(response_object)
        else:
            response_object['message'] = 'Invalid data'
            response_object['status'] = 'failed'
            return jsonify(response_object)
    except Exception as e:
        response_object['error'] = str(e)
        response_object['status'] = 'failed'
        app.logger.error(str(e))
        return jsonify(response_object)



# get histroy list with specific personID
@app.route('/api/history/<person_id>', methods=['GET'])
def get_history(person_id):
    history_list = db.session.query(HistoryList).filter_by(PersonID=person_id).all()
    
    if not history_list:
        return 'No favorite list found', 404
    
    histories = []
    for history in history_list:
        store = db.session.query(Store).get(history.StoreID)
        if store is not None:
            histories.append({'HListID': history.HListID, 'StoreID': store.StoreID, 'Name': store.Name, 'Location': store.Location, 'Distance': store.Distance, 'URL': store.URL})
    return jsonify(histories)

@app.route('/api/addHistory', methods=['POST'])
def add_history():
    response_object = {'status': 'success'}
    try:
        _json = request.json
        _StoreID = _json['StoreID']['data'][0]['StoreID'] #為甚麼回傳的json這麼複雜...
        _P1_ID = _json['P1_ID']
        if _StoreID and request.method == 'POST':
            sql = text("INSERT INTO HistoryList(PersonID, StoreID) VALUES(:PersonID, :StoreID)")
            data = {"PersonID": _P1_ID, "StoreID": _StoreID}
            db.session.execute(sql, data)
            db.session.commit()
            response_object['message'] = 'History added!'
            return jsonify(response_object)
        else:
            response_object['message'] = 'Invalid data'
            response_object['status'] = 'failed'
            return jsonify(response_object)
    except Exception as e:
        response_object['error'] = str(e)
        response_object['status'] = 'failed'
        app.logger.error(str(e))
        return jsonify(response_object)

# getStores API with personID, foodType and stationID
@app.route('/api/stores/<P_ID>/<foodType>/<stationID>', methods=['GET'])
def get_stores_PID(P_ID, foodType, stationID):
    stores = db.session.query(Store, Station.Name.label('StationName')).filter_by(Category=foodType).filter(Store.StationID==stationID).join(Station, Store.StationID==Station.StationID).limit(4)
    stores_list = []
    
    for store in stores:
        store_dict = {
            'StoreID': store.Store.StoreID,
            'Name': store.Store.Name,
            'Location': store.Store.Location,
            'Category': store.Store.Category,
            'URL': store.Store.URL,
            'Distance': store.Store.Distance,
            'StationName': store.StationName
        }
        
        # 檢查是否存在於FavoriteList中
        isFav = db.session.query(FavoriteList).filter_by(PersonID=P_ID, StoreID=store.Store.StoreID).first() is not None
        store_dict['isFav'] = isFav
        stores_list.append(store_dict)
        
    return jsonify(stores_list)

@app.route('/api/stores/<foodType>/<stationID>', methods=['GET'])
def get_stores(foodType, stationID):
    stores = db.session.query(Store, Station.Name.label('StationName')).filter_by(Category=foodType).filter(Store.StationID==stationID).join(Station, Store.StationID==Station.StationID).limit(2)
    stores_list = []
    
    for store in stores:
        store_dict = {
            'StoreID': store.Store.StoreID,
            'Name': store.Store.Name,
            'Location': store.Store.Location,
            'Category': store.Store.Category,
            'URL': store.Store.URL,
            'Distance': store.Store.Distance,
            'StationName': store.StationName
        }
        stores_list.append(store_dict)
    
    if len(stores_list) == 0:
        return jsonify({"status": "failed", "message": "No available store"})
    else:
        return jsonify({"status": "success", "storeList": stores_list})

# getStores API, without foodType and stationID
@app.route('/api/stores/<P_ID>', methods=['GET'])
def get_stores_top4(P_ID):
    stores = db.session.query(Store, Station.Name.label('StationName')).join(Station, Store.StationID==Station.StationID).limit(50)
    stores_list = []
    
    for store in stores:
        store_dict = {
            'StoreID': store.Store.StoreID,
            'Name': store.Store.Name,
            'Location': store.Store.Location,
            'Category': store.Store.Category,
            'URL': store.Store.URL,
            'Distance': store.Store.Distance,
            'StationName': store.StationName
        }
        
        # 檢查是否存在於FavoriteList中
        isFav = db.session.query(FavoriteList).filter_by(PersonID=P_ID, StoreID=store.Store.StoreID).first() is not None
        store_dict['isFav'] = isFav
        stores_list.append(store_dict)
        
    return jsonify(stores_list)



# getCommentsAPI, with StoreID
@app.route('/api/comments/<StoreID>', methods=['GET'])
def get_comments(StoreID):
    comments = db.session.query(Comment, Person.Name.label('PersonName')).filter_by(StoreID=StoreID).join(Person, Comment.PersonID==Person.PersonID).all()
    
    comments_list = []
    
    for comment, person_name in comments:
        comment_dict = {
            'PersonName': person_name,
            'Content': comment.Content
        }
        comments_list.append(comment_dict)
        
    return jsonify(comments_list)



# getAmountofPeople API, return amount of new events with P2_ID='00000000' in each station
@app.route('/api/amountofpeople', methods=['GET'])
def get_amountofpeople():
    # get all stations
    stations = db.session.query(Station).all()
    amount_list = []
    for station in stations:
        # count amount of new events with P2_ID='00000000' in a station
        amount = db.session.query(func.count(Event.P2_ID)).filter_by(StationID=station.StationID, P2_ID='0').scalar()
        status = ''
        if amount <= 3:
            status = 'success'
        elif amount <= 5:
            status = 'warning'
        else:
            status = 'error'
            
        amount_dict = {
            'StationID': station.StationID,
            'Amount': amount,
            'Status': status
        }
        amount_list.append(amount_dict)
        
    return jsonify(amount_list)     
    


@app.route('/api/storeID/<storeName>', methods=['GET'])
def get_storeID(storeName):
    print(storeName)
    store = db.session.query(Store).filter_by(Name=storeName).all()
    print('Get storeID: ', store[0].StoreID)
    store_list = [{'StoreID': store[0].StoreID }]
    return jsonify(store_list)

if __name__ == '__main__':
    app.run(port=5000)

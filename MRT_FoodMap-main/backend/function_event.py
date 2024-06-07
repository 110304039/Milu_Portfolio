import pymysql
import random

# 資料庫參數設定
db_settings = {
    "host": "localhost",
    "port": 3306,
    "user": "root",
    "password": "",
    "db": "mrt_foodmap",
    "charset": "utf8"
}

def new_event(p1_ID, time, food_type, station):
    try:
        conn = pymysql.connect(**db_settings)

        with conn.cursor() as cursor:
            # 確認這個人在一小時以內有沒有尚未配對的event
            command_time = f"select date_sub('{time}', interval 1 hour);"  # 時間
            cursor.execute(command_time)
            pre_time = cursor.fetchall()[0][0]
            command_check = f'''SELECT * FROM event
WHERE Time BETWEEN	"{pre_time}" AND "{time}"
AND P1_ID = "{p1_ID}"
AND P2_ID = "00000000"'''
            cursor.execute(command_check)
            check = cursor.fetchall()
            
            if len(check) == 0: # no events
                # new event
                command = f'''INSERT INTO event(P1_ID, P2_ID, Time, FoodType, StationID)
VALUES("{p1_ID}", "00000000", "{time}", "{food_type}", "{station}")'''
                cursor.execute(command)
                conn.commit()
                return True
            else:
                print("The person has newed an event within one hour, and hasn't paired yet")
                return False
    except Exception as ex:
        print(ex)
        return False


def random_pair(p2_ID, time, food_type='All', station='All'):
    try:
        conn = pymysql.connect(**db_settings)

        with conn.cursor() as cursor:
            # p2_ID = 00000000, 發起時間在一小時以內，食物種類跟站
            command = f'''SELECT * FROM event
WHERE P2_ID = "00000000"
AND Time between date_sub("{time}", interval 1 hour) and "{time}"
AND P1_ID != "{p2_ID}"'''
            if food_type != 'All':
                command = command + f'\nAND FoodType = "{food_type}"'
            if station != 'All':
                command = command + f'\nAND StationID = "{station}"'
            print(command)
            cursor.execute(command)
            result = cursor.fetchall()
            
            random_choice = random.choice(result)
            print('Choiced: ', random_choice)
            return random_choice
    except Exception as ex:
        print(ex)
        return False

def successfully_pair(p1_ID, p2_ID, new_event_time): #時間錯了會回傳True但資料庫不會更新
    try:
        conn = pymysql.connect(**db_settings)

        with conn.cursor() as cursor:
            # event 配對成功
            command_updateID = f'''UPDATE event
SET P2_ID = "{p2_ID}"
WHERE P1_ID = "{p1_ID}"
AND P2_ID = "00000000"
AND Time = "{new_event_time}";'''
            print("command_updateID:  \n", command_updateID, '\n')
            cursor.execute(command_updateID)
            
            command_mealpal = f'''INSERT INTO mealpal(P1_ID, P2_ID)
VALUE("{p1_ID}", "{p2_ID}")'''
            cursor.execute(command_mealpal)
            conn.commit()
            return True
    except Exception as ex:
        print(ex)
        return False
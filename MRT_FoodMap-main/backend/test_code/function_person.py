import pymysql

# 資料庫參數設定
db_settings = {
    "host": "localhost",
    "port": 3306,
    "user": "root",
    "password": "",
    "db": "mrt_foodmap",
    "charset": "utf8mb4"
}

def register(name, password, location='臺北市'):
    try:
        conn = pymysql.connect(**db_settings)

        with conn.cursor() as cursor:
            command_get_pID = "SELECT Max(PersonID) FROM person;"
            cursor.execute(command_get_pID)
            
            # PersonID 設置以及格式調整
            pID = cursor.fetchall()[0][0]
            if pID is None:
                pID = '1001'
            else:
                pID = str(1 + int(pID))
            pID = pID.zfill(8)
            
            # 將使用者資料匯入資料庫
            command = f"INSERT INTO person VALUES('{pID}', '{name}', '{password}', '{location}')"
            cursor.execute(command)
            conn.commit()
            return True
    except Exception as ex:
        print('Error Message:', ex)
        return False


def log_in(name, password):
    try:
        conn = pymysql.connect(**db_settings)
        
        with conn.cursor() as cursor:
            command_check = f"SELECT Name, Password FROM person WHERE Name = '{name}'"
            print(command_check)
            cursor.execute(command_check)
            response = cursor.fetchall()[0] # 密碼

            if password != response[1]: # 密碼是否正確
                return False
            else:
                return True
    except Exception as ex:
        print(ex)
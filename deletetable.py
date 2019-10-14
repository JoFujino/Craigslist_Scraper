import sqlalchemy as sqla

sqlEngine = sqla.create_engine('mysql+pymysql://root:@192.168.0.108/craigsdata', pool_recycle=3600)
dbConnection    = sqlEngine.connect() 

sql = str('DROP TABLE IF EXISTS scrapeddata;')
result = sqlEngine.execute(sql)
print('table has been deleted')
dbConnection.close()

class Data(object):
    def __init__(self):
        self._isReady = False
    
    @property
    def isReady(self):
        return self._isReady
    
    @isReady.setter
    def isReady(self, value):
        oldValue = self._isReady
        newValue = value
        
        if oldValue == True and newValue == False:
            print("Changed from True to False!")
            self._isReady = newValue
        
        elif oldValue == False and newValue == True:
            print("Changed from False to True!")
            self._isReady = newValue
        
        else:
            print("Value is not changed!")
            
 
data = Data()
print(f"Is data ready? { data.isReady }")

data.isReady = True
print(f"Is data ready? { data.isReady }")
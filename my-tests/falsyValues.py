def test1():
	if "": True
	else: False
	
def test2():
	if False: True
	else: False	
	
def test3():
	if None: True
	else: False	

def test4():
	if 0: True
	else: False	
	
def test5():
	if 0.0: True
	else: False
	

	
___assertFalse(test1())
___assertFalse(test2())
___assertFalse(test3())
___assertFalse(test4())
___assertFalse(test5())
class Car(object):
    def move(self):
        print("Brumm, Brumm!")

class Airplane(object):
    def move(self):
        print("Wheeeeeeeeeeeeeeeew")

class PlaneCar(Airplane):
    pass

bmway = PlaneCar()


























# bmway.__class__.__bases__ = (Car,)
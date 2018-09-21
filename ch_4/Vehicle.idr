data PowerSource = Petrol | Pedal | Electricity


data Vehicle : PowerSource -> Type where
  Bicycle : Vehicle Pedal
  Car : (fuel : Nat) -> Vehicle Petrol
  Bus :  (fuel : Nat) -> Vehicle Petrol
  Unicycle : Vehicle Pedal
  Motocycle : (fuel : Nat) -> Vehicle Petrol
  Tram : Vehicle Electricity
  Electrocar: (units: Nat) -> Vehicle Electricity

wheels: Vehicle power -> Nat
wheels Bicycle = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4
wheels Unicycle = 1
wheels (Motocycle fuel) = 2
wheels Tram = 4
wheels (Electrocar units) = 4

refuel: Vehicle Petrol -> Vehicle Petrol
refuel Bicycle impossible
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
refuel (Motocycle fuel) = Motocycle 50

recharge: Vehicle Electricity -> Vehicle Electricity
recharge Tram = Tram
recharge (Electrocar units) = Electrocar 1000



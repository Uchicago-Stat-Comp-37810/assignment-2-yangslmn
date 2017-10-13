
# coding: utf-8

# In[2]:


cars = 100 #put the number in variable cars
space_in_a_car = 4.0  # put the number in the variable space_in_a_car
drivers = 30 # put the number in the variable drivers
passengers = 90  # put the number in the variable passengers
cars_not_driven = cars - drivers # use the variable defined above
cars_driven = drivers # use the variable defined above
carpool_capacity = cars_driven * space_in_a_car # multiply is also available
average_passengers_per_car = passengers / cars_driven  # divide is also availabe


print("There are", cars, "cars available.") # print the strings with the variable defined above
print("There are only", drivers, "drivers available.") # print the strings with the variable defined above
print("There will be", cars_not_driven, "empty cars today.")# print the strings with the variable defined above
print("We can transport", carpool_capacity, "people today.")# print the strings with the variable defined above
print("We have", passengers, "to carpool today.")# print the strings with the variable defined above
print("We need to put about", average_passengers_per_car,
      "in each car.")# print the strings with the variable defined above


# In[3]:


Traceback (most recent call last):
  File "ex4.py", line 8, in <module>
    average_passengers_per_car = car_pool_capacity / passenger
NameError: name 'car_pool_capacity' is not defined
    # This happens when the name of variable is not the same- the name should be exactly same, without any small difference. 

#more study drills
#1)
# 4 and 4.0 are the same when we calculate via python.
# 4) = is used to assign a content to a variable
# 6)
i<-11
j<- 21
x<- 3
print(i+j+x)

from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import mean_squared_error
from sklearn.model_selection import GridSearchCV
from datetime import datetime
import pandas as pd
import numpy as np

class FaceoffRandomForest:

    def __init__(self):
        self.data = self.read_data()

    def read_data(self):
        data_2018 = pd.read_csv("Full2018.csv")
        data_2019 = pd.read_csv("Full2019.csv")
        data = data_2018.append(data_2019)
        return(data)

    def condition_data(self, data):
        return(data)

    def learn(self):
        print(self.data.columns[25:27])
        cols_of_interest_1 = list(self.data.columns[59:64])
        print(cols_of_interest_1)
        cols_of_interest_2 = list(self.data.columns[66:71])
        print(cols_of_interest_2)
        cols_of_interest = cols_of_interest_1 + cols_of_interest_2
        print(cols_of_interest)
        cols_of_interest.append('faceoff_winning_team_xG_since_faceoff')
        data_no_na = self.data[cols_of_interest].dropna()
        cols_of_interest.remove('faceoff_winning_team_xG_since_faceoff')
        # X = self.data.loc[:, self.data.columns != 'faceoff_winning_team_xG_since_faceoff']
        X = data_no_na[cols_of_interest]
        #y = np.random.randint(0, 10, size = len(data_no_na['faceoff_winning_team_xG_since_faceoff']))
        y = data_no_na['faceoff_winning_team_xG_since_faceoff']
        x_train, x_test, y_train, y_test = train_test_split(X, y, test_size=0.25, random_state=42)
        rf = RandomForestRegressor(n_estimators = 300, max_features = 'sqrt', max_depth = 5, random_state = 18).fit(x_train, y_train)

        prediction = rf.predict(x_test)
        mse = mean_squared_error(y_test, prediction)
        rmse = mse ** .5
        print(mse)
        print(rmse)

        grid = {
            'n_estimators': [200],
            'max_features': ['sqrt', 'log2'],
            'max_depth': [5],
            'random_state': [18]
        }
        # grid = {
        #     'n_estimators': [200, 300, 400, 500],
        #     'max_features': ['sqrt', 'log2'],
        #     'max_depth': [3, 4, 5, 6, 7],
        #     'random_state': [18]
        # }
        ## show start time
        print(datetime.now())
        ## Grid Search function
        CV_rfr = GridSearchCV(estimator=RandomForestRegressor(), param_grid=grid, cv=5)
        CV_rfr.fit(x_train, y_train)
        ## show end time
        print(datetime.now())

def main():
    frf = FaceoffRandomForest()
    frf.learn()

if __name__ == "__main__":
    main()
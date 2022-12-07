import sklearn
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import mean_squared_error
from sklearn.model_selection import GridSearchCV
from sklearn.decomposition import PCA
from datetime import datetime
import pandas as pd
import numpy as np

class FaceoffRandomForest:

    def __init__(self):
        self.data = self.read_data()

    def read_data(self):
        # data_2018 = pd.read_csv("Full2018.csv")
        # data_2019 = pd.read_csv("Full2019.csv")
        # data = data_2018.append(data_2019)
        data = pd.read_csv('C:/Users/Tad/Documents/faceoffs/data_imputed.csv')
        return(data)

    def condition_data(self, data):
        return(data)

    def pca(self):
        data = self.read_data()
        print("data read")
        x_cols = self.data.columns[23:]
        x_cols.remove('faceoff_winning_team_xG_since_faceoff')
        print(x_cols)
        x = data[x_cols]
        y = data['faceoff_winning_team_xG_since_faceoff']
        pca = PCA(n_components = 100)
        print("pca fitting")
        principal_components = pca.fit_transform(x)
        print(principal_components)
        principal_components.to_csv("principal_components_python.csv")
        return(principal_components)

    def learn(self):
        cols_of_interest = self.data.columns[26:]
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
    frf.pca()

if __name__ == "__main__":
    main()
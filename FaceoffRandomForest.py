from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestRegressor
import pandas as pd

class FaceoffRandomForest:

    def read_data(self):
        data = pd.read_csv("_____.csv")
        return(data)

    def condition_data(self, data):
        return(data)

    def learn(self, data):
        X = data.loc[:, data.columns != 'cumulative_xG']
        y = data['cumulative_xG']
        x_train, x_test, y_train, y_test = train_test_split(X, y, test_size=0.25, random_state=42)
        rf = RandomForestRegressor(n_estimators = 300, max_features = 'sqrt', max_depth = 5, random_state = 18).fit(x_train, y_train)

        prediction = rf.predict(x_test)
        mse = mean_squared_error(y_test, prediction)
        rmse = mse ** .5
        print(mse)
        print(rmse)

        grid = {
            'n_estimators': [200, 300, 400, 500],
            'max_features': ['sqrt', 'log2'],
            'max_depth': [3, 4, 5, 6, 7],
            'random_state': [18]
        }
        ## show start time
        print(datetime.now())
        ## Grid Search function
        CV_rfr = GridSearchCV(estimator=RandomForestRegressor(), param_grid=grid1, cv=5)
        CV_frf.fit(x_train, y_train)
        ## show end time
        print(datetime.now())
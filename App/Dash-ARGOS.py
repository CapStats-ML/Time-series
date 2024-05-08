import pandas as pd
import plotly.graph_objects as go
import dash
from dash import dcc, html, Input, Output

# Cargar el archivo CSV en un DataFrame de pandas
archivo_csv = r'C:\Users\HP\OneDrive\Documentos\DOCUMENTOS PERSONAJES\CESAR\Series\Pro_Series\Series-de-Tiempo\Datos\G_ARGOS.csv'
ARGOS = pd.read_csv(archivo_csv, sep=';')

# Convertir la columna de fecha al formato de fecha
ARGOS['Fecha'] = pd.to_datetime(ARGOS['Fecha'], format='%d/%m/%Y')

# Convertir las comas en los puntos en las columnas numéricas
columnas_numericas = ['Último', 'Apertura', 'Máximo', 'Mínimo']
ARGOS[columnas_numericas] = ARGOS[columnas_numericas].replace('\,', '.', regex=True)
ARGOS[columnas_numericas] = ARGOS[columnas_numericas].replace('\.', '', regex=True)

# Convertir las columnas numéricas al tipo float
ARGOS[columnas_numericas] = ARGOS[columnas_numericas].astype(float)

# Dividir entre 1000 para eliminar los ceros adicionales
ARGOS[columnas_numericas] = ARGOS[columnas_numericas].div(100000)

# Reordenar el DataFrame por fecha de la más antigua a la más reciente
ARGOS = ARGOS.sort_values(by='Fecha')

# Reiniciar el índice
ARGOS = ARGOS.reset_index(drop=True)

# Crear la aplicación Dash
app = dash.Dash(__name__)

# Definir el diseño de la aplicación con márgenes
app.layout = html.Div(style={'margin': '100px'}, children=[
    html.H1('PROYECTO DE CLASE - ANÁLISIS DE SERIES DE TIEMPO', style={'textAlign': 'center'}),
    html.H2('SERIE: ANÁLISIS DE COTIZACIONES EN BOLSA - GRUPO ARGOS', style={'textAlign': 'center'}),
    
    # Primer bloque de HTML
    html.Div(style={'margin': '40px'}, children=[
        html.H2('SERIE DE TIEMPO GRUPO ARGOS', style={'textAlign': 'center'}),
        html.P('GRUPO ARGOS es una empresa de construcción y cemento en Colombia. Lo que se muestra a continuación es una serie de tiempo de las cotizaciones de la empresa en la bolsa de valores de Colombia. El plan es explorar la serie de tiempo y aplicar los métodos aprendidos en clase para analizarla, describir sus propiedades y hacer predicciones.'),
        dcc.Graph(id='time-series-graph1'),
        dcc.Dropdown(
            id='variable-dropdown1',
            options=[
                {'label': 'Apertura', 'value': 'Apertura'},
                {'label': 'Último', 'value': 'Último'},
                {'label': 'Máximo', 'value': 'Máximo'},
                {'label': 'Mínimo', 'value': 'Mínimo'}
            ],
            value='Apertura'
        )
    ]),
    
    # Segundo bloque de HTML
    html.Div(style={'margin': '40px'}, children=[
        html.P('Del gráfico anterior podemos concluir que todas las variables se comportan de manera similar en el tiempo explorado, por lo cual la elección de la variable en la que nos centraremos no tendrá una influencia positiva o negativa en nuestro estudio. Sin embargo, podemos notar que entre el 2021 y 2022 hubo una caída muy brusca del precio de las acciones del Grupo ARGOS, lo cual sí podrá afectar de manera directa los resultados que obtengamos.'),
        html.P('**Recomendación:** Tratar de realizar la exploración de la serie antes de la caída y comparar resultados.'),
        html.Hr(),  # Línea horizontal para separar el texto de desarrollo
        html.H2('Elección de la Variable y Desarrollo de Metodologías', style={'color': '#011f4b', 'textAlign': 'center'}),
        html.P("En la parte anterior del código pudimos ver cómo todas las variables poseen un comportamiento bastante parecido, lo cual tiene sentido en el ámbito en el cual nos encontramos, dado que estamos hablando del precio de las acciones de una compañía y este suele comportarse de forma muy similar hablando en términos de las variables del precio de la apertura, el precio máximo y mínimo o el cierre."),
        html.P("Por esto hemos decidido que la variable elegida para desarrollar el proyecto será la **Apertura** o mejor dicho el precio de la apertura diaria del valor de la acción en la bolsa para el Grupo Argos, en miles de pesos colombianos. Teniendo esto en cuenta, también podríamos aplicar el siguiente desarrollo a las otras variables pero eso no será tema principal en este documento."),
        dcc.Graph(id='time-series-graph2'),
        dcc.Dropdown(
            id='variable-dropdown2',
            options=[
                {'label': 'Apertura', 'value': 'Apertura'},
            ],
            value='Apertura'
        )
    ])
])


# Definir la función para actualizar el primer gráfico
@app.callback(
    Output('time-series-graph1', 'figure'),
    [Input('variable-dropdown1', 'value')]
)
def update_graph1(selected_variable):
    fig = go.Figure()
    fig.add_trace(go.Scatter(x=ARGOS['Fecha'], y=ARGOS[selected_variable], mode='lines', name=selected_variable))
    fig.update_layout(
        title=f'Serie de tiempo variable {selected_variable}',
        xaxis_title='Fecha',
        yaxis_title='Valor',
        xaxis_rangeslider_visible=True
    )
    return fig

# Definir la función para actualizar el segundo gráfico
@app.callback(
    Output('time-series-graph2', 'figure'),
    [Input('variable-dropdown2', 'value')]
)
def update_graph2(selected_variable):
    fig = go.Figure()
    fig.add_trace(go.Scatter(x=ARGOS['Fecha'], y=ARGOS['Apertura'], mode='lines', name='Apertura'))
    fig.update_layout(
        title=f'Serie de tiempo variable Apertura',
        xaxis_title='Fecha',
        yaxis_title='Valor',
        xaxis_rangeslider_visible=True
    )
    return fig

# Ejecutar la aplicación
if __name__ == '__main__':
    app.run_server(debug=True)
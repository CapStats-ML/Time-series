import streamlit as st

st.set_page_config(
    page_title="Serie Grupo Argos",
    layout="wide",
    initial_sidebar_state="expanded",
    menu_items={
        'Get Help': 'https://www.extremelycoolapp.com/help',
        'Report a bug': "https://www.extremelycoolapp.com/bug",
        'About': "# This is a header. This is an *extremely* cool app!"
    }
)


# Título principal centrado
st.markdown('<center><h1 style="color: #011f4b;">PROYECTO DE CLASE - ANÁLISIS DE SERIES DE TIEMPO</h1></center>', unsafe_allow_html=True)

# Subtítulo centrado
st.markdown('<center><h2 style="color: #03396c;">SERIE: ANÁLISIS DE COTIZACIONES EN BOLSA - ACCIONES DE ARGOS</h2></center>', unsafe_allow_html=True)

st.write('''
         La base de datos fue tomada de la pagina _https://es.investing.com/equities/grupoargos-historical-data_ la cual proporciona
         estos datos de manera gratuita al publico, la inspiracion para desarrollar este trabajo con esta base es conocer como se comportan
         los metodos aprendidos en clase en un enfoque econocimo y sobre todo en el area de las acciones, con lo cual decidimos tomar 
         los datos de un grupo empresarial Colombiano y realizar el analisis correspondiente para desarrollar el proyecto para la clase 
         de series de tiempo.
         ''')

st.markdown('''
    <span style="color: #03396c;">Descripción de la base:</span>
    <ul>
        <li>Acciones <span style="color: #03396c;">**DIARIAS**</span> del Grupo ARGOS</li>
        <li>3401 Datos tomados (Desde el 7/11/2014 Hasta 28/02/2024)</li>
        <li>Variables: Fecha, Último, <span style="color: #03396c;">**_Apertura_**</span>, Máximo, Mínimo, Vol, %Var.</li>
    </ul>
''', unsafe_allow_html=True)

st.header('ANALISIS Y DESCRIPCION DE LA SERIE', divider='grey')
st.header('ARBOLES DE DECISION', divider='grey')
st.header('REDES NEURONALES DE 1 CAPA', divider='grey')
st.header('REDES NEURONALES MULTICAPA', divider='grey')
st.header('REDES NEURONALES RECURRENTES Y EVALUACION DE MODELOS', divider='grey')


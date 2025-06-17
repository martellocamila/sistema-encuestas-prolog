% ----------------------------------------------------------------
% Sistema de encuestas para valoracion de libros de una editorial
% ----------------------------------------------------------------

% Base de datos dinamica para encuestas
:- dynamic encuesta/8. 

% ---------------------------
% BASE DE CONOCIMIENTOS 
% ---------------------------

% Productos (Libros de la editorial)
% producto(ID, Titulo, Genero)
producto(1, 'Vuelvete Imparable', autoayuda).
producto(2, 'Reina Roja', novela_negra).
producto(3, 'La ciudad de las bestias', fantasia).
producto(4, 'Bajo el cielo de tus ojos', romantica).
producto(5, 'Los pilares de la Tierra', historica).

% Encuestas
% encuesta(ID, ProductoID, RangoEdad, Genero, Acepta, RazonAceptacion,
% RazonNoAceptacion, DispuestoAPagar)
% - RangoEdad: r18_25, r26_35,
% r36_50, r51_mas
% - Genero: masculino, femenino, otro
% - Acepta: si, no 
% - RazonAceptacion: trama_atrapante, genero_favorito,
% personajes_interesantes, estilo_escritura, ambientacion_interesante,
% completo
% - RazonNoAceptacion: aburrido, no_me_gusta_el_genero,
% dificil_de_entender, personajes_malos, predecible, estilo_escritura 
% - DispuestoAPagar: menos_de_10000, entre_10000_y_15000,
% entre_15000_y_20000, entre_20000_y_30000, mas_de_30000

% Encuestas sobre el producto 1 - 'Vuelvete Imparable'
encuesta(1, 1, r36_50, masculino, si, genero_favorito, _, mas_de_30000).
encuesta(2, 1, r26_35, masculino, no, _, estilo_escritura, menos_de_10000).
encuesta(3, 1, r36_50, otro, si, estilo_escritura, _, entre_10000_y_15000).
encuesta(4, 1, r51_mas, femenino, no, _, estilo_escritura, entre_15000_y_20000).
encuesta(5, 1, r18_25, masculino, si, trama_atrapante, _, entre_20000_y_30000).
encuesta(6, 1, r26_35, femenino, si, completo, _, entre_20000_y_30000).
encuesta(7, 1, r36_50, masculino, no, _, estilo_escritura, entre_10000_y_15000).
encuesta(8, 1, r18_25, otro, si, estilo_escritura, _, menos_de_10000).
encuesta(9, 1, r51_mas, femenino, no, _, predecible, entre_15000_y_20000).
encuesta(10, 1, r36_50, masculino, si, genero_favorito, _, mas_de_30000).

% Encuestas sobre el producto 2 - 'Reina Roja'
encuesta(11, 2, r18_25, femenino, si, trama_atrapante, _, entre_15000_y_20000).
encuesta(12, 2, r26_35, masculino, si, trama_atrapante, _, entre_20000_y_30000).
encuesta(13, 2, r36_50, masculino, no, _, no_me_gusta_el_genero, menos_de_10000).
encuesta(14, 2, r51_mas, otro, si, trama_atrapante, _, mas_de_30000).
encuesta(15, 2, r18_25, femenino, si, _, genero_favorito, entre_10000_y_15000).
encuesta(16, 2, r18_25, masculino, si, genero_favorito, _, mas_de_30000).
encuesta(17, 2, r26_35, femenino, no, _, aburrido, menos_de_10000).
encuesta(18, 2, r36_50, otro, si, personajes_interesantes, _, entre_15000_y_20000).
encuesta(19, 2, r51_mas, masculino, no, _, personajes_malos, entre_20000_y_30000).
encuesta(20, 2, r26_35, otro, si, trama_atrapante, _, entre_10000_y_15000).

% Encuestas sobre el producto 3 - 'La ciudad de las bestias'
encuesta(21, 3, r18_25, femenino, si, personajes_interesantes, _, entre_20000_y_30000).
encuesta(22, 3, r26_35, masculino, no, _, no_me_gusta_el_genero, menos_de_10000).
encuesta(23, 3, r36_50, otro, si, ambientacion_interesante, _, entre_15000_y_20000).
encuesta(24, 3, r51_mas, femenino, no, _, dificil_de_entender, entre_10000_y_15000).
encuesta(25, 3, r18_25, masculino, si, trama_atrapante, _, entre_15000_y_20000).
encuesta(26, 3, r26_35, otro, no, _, predecible, menos_de_10000).
encuesta(27, 3, r36_50, femenino, si, genero_favorito, _, mas_de_30000).
encuesta(28, 3, r51_mas, masculino, si, completo, _, entre_20000_y_30000).
encuesta(29, 3, r18_25, otro, no, _, personajes_malos, entre_10000_y_15000).
encuesta(30, 3, r26_35, femenino, si, estilo_escritura, _, mas_de_30000).

% Encuestas sobre el producto 4 - 'Bajo el cielo de tus ojos'
encuesta(31, 4, r18_25, femenino, si, genero_favorito, _, entre_15000_y_20000).
encuesta(32, 4, r18_25, masculino, no, _, aburrido, menos_de_10000).
encuesta(33, 4, r18_25, femenino, si, trama_atrapante, _, entre_20000_y_30000).
encuesta(34, 4, r51_mas, otro, no, _, predecible, entre_10000_y_15000).
encuesta(35, 4, r18_25, masculino, no, personajes_interesantes, _, mas_de_30000).
encuesta(36, 4, r18_25, femenino, si, _, genero_favorito, entre_15000_y_20000).
encuesta(37, 4, r36_50, masculino, si, estilo_escritura, _, entre_15000_y_20000).
encuesta(38, 4, r51_mas, femenino, si, genero_favorito, _, mas_de_30000).
encuesta(39, 4, r18_25, otro, no, _, no_me_gusta_el_genero, entre_10000_y_15000).
encuesta(40, 4, r26_35, masculino, si, ambientacion_interesante, _, entre_20000_y_30000).

% Encuestas sobre el producto 5 - 'Los pilares de la tierra'
encuesta(41, 5, r18_25, masculino, no, _, aburrido, menos_de_10000).
encuesta(42, 5, r26_35, femenino, no, _, dificil_de_entender, entre_10000_y_15000).
encuesta(43, 5, r36_50, otro, no, _, aburrido, menos_de_10000).
encuesta(44, 5, r51_mas, femenino, no, _, aburrido, entre_15000_y_20000).
encuesta(45, 5, r18_25, masculino, no, _, aburrido, menos_de_10000).
encuesta(46, 5, r26_35, femenino, si, trama_atrapante, _, entre_15000_y_20000).
encuesta(47, 5, r36_50, masculino, no, _, aburrido, menos_de_10000).
encuesta(48, 5, r51_mas, otro, no, _, dificil_de_entender, entre_10000_y_15000).
encuesta(49, 5, r18_25, femenino, no, _, predecible, menos_de_10000).
encuesta(50, 5, r26_35, masculino, si, completo, _, entre_20000_y_30000).

% ---------------------------
% REGLAS DE CONSULTAS
% ---------------------------

% Listar productos
lista_productos:-
    format('Productos: ~n'), % Encabezado
    producto(ID, Titulo, Genero), % Recupera cada producto
    format('  ID: ~w, Titulo: ~w, Genero: ~w~n', [ID, Titulo, Genero]), % Formatea la salida
    fail. % Fuerza a Prolog a retroceder y recuperar todos los productos
lista_productos. 

% Listar encuestas por producto
lista_encuestas_por_producto(ProductoID):-
    format('Encuestas para el producto ID ~w:~n', [ProductoID]), % Encabezado
    encuesta(ID, ProductoID, RangoEdad, Genero, Acepta, RazonAceptacion, RazonNoAceptacion, DispuestoAPagar), % Recupera cada encuesta
    format('  ID: ~w, RangoEdad: ~w, Genero: ~w, Acepta: ~w, RazonAceptacion: ~w, RazonNoAceptacion: ~w, DispuestoAPagar: ~w~n',
           [ID, RangoEdad, Genero, Acepta, RazonAceptacion, RazonNoAceptacion, DispuestoAPagar]), % Formatea la salida
    fail. % Fuerza a Prolog a retroceder y recuperar todas las encuestas
lista_encuestas_por_producto(_).

% Contar total de encuestas
total_encuestas(Total) :-
    findall(ID, encuesta(ID, _, _, _, _, _, _, _), Encuestas), % Recupera todas las encuestas
    length(Encuestas, Total). % Cuenta la longitud de la lista Encuestas

% Contar el total de encuestas aceptadas
total_encuestas_aceptadas(Total) :-
    findall(ID, encuesta(ID, _, _, _, si, _, _, _), EncuestasAceptadas), % Recupera las encuestas aceptadas
    length(EncuestasAceptadas, Total). % Cuenta la longitud de la lista EncuestasAceptadas

% Contar el total de encuestas no aceptadas
total_encuestas_no_aceptadas(Total) :-
    findall(ID, encuesta(ID, _, _, _, no, _, _, _), EncuestasNoAceptadas), % Recupera las encuestas no aceptadas
    length(EncuestasNoAceptadas, Total). % Cuenta la longitud de la lista EncuestasNoAceptadas

% Contar encuestas aceptadas por producto
encuestas_aceptadas_por_producto(ProductoID, Total) :-
    findall(ID, encuesta(ID, ProductoID, _, _, si, _, _, _), Encuestas), % Recupera las encuestas aceptadas para el producto
    length(Encuestas, Total). % Cuenta la longitud de la lista Encuestas

% Buscar el producto mas aceptado
producto_mas_aceptado(ProductoID, Max) :-
    findall((Cant, P), (producto(P, _, _), encuestas_aceptadas_por_producto(P, Cant)), Pares), % Encuentra pares (cantidad, producto), para cada producto con ID P obtiene la cantidad Cant de encuestas con respuesta "sí" para ese producto
    sort(Pares, Ordenados), % Ordena por cantidad ascendente
    last(Ordenados, (Max, ProductoID)). % Toma el último (mayor cantidad)

% Buscar el producto menos aceptado
producto_menos_aceptado(ProductoID, Min) :-
    findall((Cant, P), (producto(P, _, _), encuestas_aceptadas_por_producto(P, Cant)), Pares),
    sort(Pares, Ordenados), % Ordena por cantidad ascendente
    Ordenados = [(Min, ProductoID)|_]. % Toma el primero (menor cantidad)

% Contar ocurrencias de un elemento en una lista (AUXILIAR)
contar_ocurrencias(_, [], 0). % Caso base: lista vacía  
contar_ocurrencias(X, [X|T], N) :- % Si el elemento es igual al primero de la lista
    contar_ocurrencias(X, T, N1), % Cuenta las ocurrencias en el resto de la lista
    N is N1 + 1. % Incrementa el contador
    contar_ocurrencias(X, [Y|T], N) :- % Si el elemento no es igual al primero de la lista
    X  \= Y, % Asegura que no se cuenta el elemento
    contar_ocurrencias(X, T, N). % Cuenta las ocurrencias en el resto de la lista

% Obtener todas las combinaciones (Edad, Genero) para encuestas aceptadas (AUXILIAR)
combos_edad_genero_acepta(ProductoID, Combo) :-
  findall((Edad, Genero), 
          (encuesta(_, ProductoID, Edad, Genero, si, _, _, _)), % Encuentra encuestas aceptadas
          Combo). % Retorna la combinación de Edad y Genero

% Calcular la combinacion mas frecuente
rango_genero_mas_frecuente_acepta(ProductoID, Edad, Genero) :-
    combos_edad_genero_acepta(ProductoID, Lista), % Obtiene todas las combinaciones de Edad y Genero
    setof((N, E, G), (member((E, G), Lista), contar_ocurrencias((E, G), Lista, N)), Frecuencias), % Cuenta las ocurrencias de cada combinación
    sort(Frecuencias, Ordenadas), % Ordena las combinaciones por frecuencia ascendente
    last(Ordenadas, (_, Edad, Genero)).  % Obtiene la combinación con mayor ocurrencia

% Obtener todas las combinaciones (Edad, Genero) para encuestas NO aceptadas (AUXILIAR)
combos_edad_genero_no_acepta(ProductoID, Combo) :-
    findall((Edad, Genero),
            encuesta(_, ProductoID, Edad, Genero, no, _, _, _),
            Combo).

% Calcular la combinacion mas frecuente de NO aceptadas
rango_genero_mas_frecuente_no_acepta(ProductoID, Edad, Genero) :-
    combos_edad_genero_no_acepta(ProductoID, Lista), % Obtiene todas las combinaciones de Edad y Genero
    setof((N, E, G), (member((E, G), Lista), contar_ocurrencias((E, G), Lista, N)), Frecuencias), % Cuenta las ocurrencias de cada combinación
    sort(Frecuencias, Ordenadas), % Ordena las combinaciones por frecuencia ascendente
    last(Ordenadas, (_, Edad, Genero)).  % Obtiene la combinación con mayor ocurrencia

% Obtener todas las razones para un producto y un estado (acepta o no acepta) (AUXILIAR)
razones_por_estado(ProductoID, si, Razones) :-
    findall(Razon,
            encuesta(_, ProductoID, _, _, si, Razon, _, _),
            Razones).

razones_por_estado(ProductoID, no, Razones) :-
    findall(Razon,
            encuesta(_, ProductoID, _, _, no, _, Razon, _),
            Razones).

% Obtener la razon mas mencionada en una lista de razones (AUXILIAR)
razon_mas_mencionada(Razones, RazonMasComun) :-
    sort(Razones, RazonesUnicas), % elimina duplicados
    findall((N, R), 
            (member(R, RazonesUnicas), contar_ocurrencias(R, Razones, N)), 
            Frecuencias),
    sort(Frecuencias, Ordenadas),
    last(Ordenadas, (_, RazonMasComun)).

% Obtener la razon principal de aceptacion de un producto
razon_principal_aceptacion(ProductoID, Razon) :-
    razones_por_estado(ProductoID, si, Razones),
    razon_mas_mencionada(Razones, Razon).

% Obtener la razon principal de no aceptacion de un producto
razon_principal_no_aceptacion(ProductoID, Razon) :-
    razones_por_estado(ProductoID, no, Razones),
    razon_mas_mencionada(Razones, Razon).

% Obtener lo que estan dispuestos a pagar los encuestados que aceptan un producto
precio_aceptacion(ProductoID, Precios) :-
    findall(Precio,
            encuesta(_, ProductoID, _, _, si, _, _, Precio),
            Precios).

% Contar ocurrencias de cada precio para un producto dado
conteo_precios_aceptacion(ProductoID, Conteos) :-
    precio_aceptacion(ProductoID, Precios),
    sort(Precios, PreciosUnicos), % Elimina duplicados
    findall((Precio, Count),
            (member(Precio, PreciosUnicos),
             contar_ocurrencias(Precio, Precios, Count)),
            Conteos).

% Agregar una nueva encuesta dinámica en memoria
agregar_encuesta(ProductoID, RangoEdad, Genero, Acepta, RazonAceptacion, RazonNoAceptacion, DispuestoAPagar) :-
    % Generar un nuevo ID para la encuesta
    findall(ID, encuesta(ID, _, _, _, _, _, _, _), IDs), % Recupera todos los IDs de encuestas existentes
    length(IDs, NuevoID), % Calcula el nuevo ID como la longitud de la lista de IDs
    NuevoID1 is NuevoID + 1, % Incrementa el ID para la nueva encuesta
    assertz(encuesta(NuevoID1, ProductoID, RangoEdad, Genero, Acepta, RazonAceptacion, RazonNoAceptacion, DispuestoAPagar)), % Agrega la nueva encuesta a la base de datos
    format('Se agrego una nueva encuesta con ID ~w.~n', [NuevoID1]).
  
% Guardar la base de datos en un archivo
guardar_cambios(Archivo) :-
    tell(Archivo), % Abre el archivo para escritura
    listing(producto), % Lista los productos
    listing(encuesta), % Lista las encuestas
    told. % Cierra el archivo

% Mostrar reporte general de encuestas
reporte_general :-
    total_encuestas(Total),
    total_encuestas_aceptadas(TotalAceptadas),
    total_encuestas_no_aceptadas(TotalNoAceptadas),
    format('Total de encuestas: ~w~n', [Total]),
    format('Total de encuestas aceptadas: ~w~n', [TotalAceptadas]),
    format('Total de encuestas no aceptadas: ~w~n', [TotalNoAceptadas]),
    producto_mas_aceptado(ProductoIDMax, Max),
    format('Producto mas aceptado: ID ~w con ~w aceptaciones~n', [ProductoIDMax, Max]),
    producto_menos_aceptado(ProductoIDMin, Min),
    format('Producto menos aceptado: ID ~w con ~w aceptaciones~n', [ProductoIDMin, Min]).

% Mostrar reporte por producto
reporte_por_producto(ProductoID) :-
    format('Reporte para el producto ID ~w:~n', [ProductoID]),
    encuestas_aceptadas_por_producto(ProductoID, TotalAceptadas),
    encuestas_aceptadas_por_producto(ProductoID, TotalNoAceptadas),
    format('Total de encuestas aceptadas: ~w~n', [TotalAceptadas]),
    format('Total de encuestas no aceptadas: ~w~n', [TotalNoAceptadas]),
    rango_genero_mas_frecuente_acepta(ProductoID, Edad, Genero),
    format('Rango de edad y genero mas frecuente entre aceptaciones: ~w, ~w~n', [Edad, Genero]),
    rango_genero_mas_frecuente_no_acepta(ProductoID, EdadNo, GeneroNo),
    format('Rango de edad y genero mas frecuente entre no aceptaciones: ~w, ~w~n', [EdadNo, GeneroNo]),
    razon_principal_aceptacion(ProductoID, RazonAceptacion),
    format('Razon principal de aceptacion: ~w~n', [RazonAceptacion]),
    razon_principal_no_aceptacion(ProductoID, RazonNoAceptacion),
    format('Razon principal de no aceptacion: ~w~n', [RazonNoAceptacion]),
    conteo_precios_aceptacion(ProductoID, ConteosPrecios),
    format('Conteo de precios dispuestos a pagar por los que aceptan el producto: ~n'),
    forall(member((Precio, Count), ConteosPrecios), 
           format('  Precio: ~w - Cantidad: ~w~n', [Precio, Count])).
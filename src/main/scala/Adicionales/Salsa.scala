package Adicionales

import Servicios.GeneradorReferencia

class Salsa(nombreCreacion : String)
{
    /*Atributos*/
    private var _referencia : String = GeneradorReferencia.crearReferencia()
    private var _nombre : String = nombreCreacion

    /*Getters*/
    def referencia = _referencia
    def nombre = _nombre

    /*Setters*/
    def nombre_= (nombreNuevo : String) = _nombre = nombreNuevo
}
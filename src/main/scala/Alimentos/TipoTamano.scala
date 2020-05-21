package Alimentos

import Servicios.GeneradorReferencia

class TipoTamano
{
    /*Atributos*/
    var _idTipoTamano : String = GeneradorReferencia.crearReferencia()
    var _descripcion : String = _

    /*Getters*/
    def idTipoTamano = _idTipoTamano
    def descripcion = _descripcion

    /*Setters*/
    def descripcion_= (nuevaDescripcion : String) = _descripcion = nuevaDescripcion

    /*Métodos*/
}
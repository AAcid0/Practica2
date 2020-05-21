package Alimentos

import InsumosPlasticos.InsumoCocina

abstract class Alimento extends InsumoCocina
{
    /*Atributos*/
    protected var _tamano : TipoTamano
    protected var _descripcion : String
    protected var _referencia : String
    protected var _tipoAlimento : String

    /*Getters*/
    def tamano = _tamano
    def descripcion = _descripcion
    def referencia = _referencia
    def tipoAlimento = _tipoAlimento

    /*Setters*/
    def tamano_= (nuevoTamano : TipoTamano) = _tamano = nuevoTamano
    def descripcion_= (nuevaDescripcion : String) = _descripcion = nuevaDescripcion
    def referencia_= (nuevaReferencia : String) = _referencia = nuevaReferencia
    def tipoAlimento_= (nuevoTipo : String) = _tipoAlimento = nuevoTipo    
}

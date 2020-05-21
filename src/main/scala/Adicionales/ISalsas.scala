package Adicionales

trait ISalsas
{
    /*Atributos*/
    var _tieneSalsas : Boolean
    var _salsas : List[Salsa]

    /*Métodos*/
    def agregarSalsa(salsa : Salsa) : Unit
}
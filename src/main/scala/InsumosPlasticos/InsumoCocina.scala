package InsumosPlasticos

trait InsumoCocina
{
    /*Atributos*/
    protected var _costo : Double

    /*Métodos*/
    def setCosto(nuevoCosto : Double) = _costo = nuevoCosto
    def calcularCosto() : Double
}
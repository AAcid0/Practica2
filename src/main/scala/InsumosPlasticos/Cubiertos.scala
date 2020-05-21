package InsumosPlasticos

class Cubiertos extends InsumoCocina
{
    /*Atributos*/
    override var _costo : Double = 1000

    /*Métodos*/
    def calcularCosto() : Double = 
    {
        return _costo
    }
}
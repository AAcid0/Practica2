package InsumosPlasticos

class TapaBebida extends InsumoCocina
{
    /*Atributos*/
    override var _costo : Double = 200

    /*Métodos*/
    def calcularCosto() : Double = 
    {
        return _costo
    }
}
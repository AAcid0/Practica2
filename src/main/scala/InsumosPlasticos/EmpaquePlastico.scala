package InsumosPlasticos

class EmpaquePlastico extends InsumoCocina
{
    /*Atributos*/
    override var _costo : Double = 500

    /*Métodos*/
    def calcularCosto() : Double = 
    {
        return _costo
    }
}
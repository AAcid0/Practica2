import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import Alimentos.TipoTamano
import Alimentos.Hamburguesa

class HamburguesaTest extends FunSuite with BeforeAndAfter
{
    var tipoGrande : TipoTamano = new TipoTamano
    var tipoMediano : TipoTamano = new TipoTamano
    var tipoPequeno : TipoTamano = new TipoTamano

    before
    {
        tipoGrande.descripcion = "Grande"    
        tipoMediano.descripcion = "Mediana"    
        tipoPequeno.descripcion = "Pequeño"    
    }

    test("Al crear una hamburguesa Mediana 'Ranchera' su costo será de 25000")
    {
        var hamburguesa : Hamburguesa = new Hamburguesa(tipoMediano, "Ranchera", 25000)
        hamburguesa.esAgrandable = false
        assert(hamburguesa.calcularCosto() == 25000)
    }

    test ("Al crear, se asigna un ID automáticamente")
    {
        var hamburguesa : Hamburguesa = new Hamburguesa(tipoMediano, "Ranchera", 25000)
        assert(hamburguesa.referencia != "")
    }
    
}
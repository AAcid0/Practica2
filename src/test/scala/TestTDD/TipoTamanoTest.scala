import org.scalatest.FunSuite
import Alimentos.TipoTamano

class TipoTamanoTest extends FunSuite
{
    var tipoTamano : TipoTamano = new TipoTamano

    test ("Al crear, se asigna un ID automáticamente")
    {
        assert(tipoTamano.idTipoTamano != "")
    }

    test("El ID debe tener 6 caracteres de longitud")
    {
        val longitud = tipoTamano.idTipoTamano.length()
        assert(longitud == 6)
    }

    test("Cuando se asigna la descripción, el valor de la descripción no es vacio") (pending)

    test("El tipoTamano tiene como ID un número") 
    {
        fail()
    }

    test("Cuando se asigna Grande a la descripción, se debe mostrar 'Grande' al solicitarlo")
    {
        tipoTamano.descripcion = "Grande"
        assertResult("Grande")
        {
            tipoTamano.descripcion
        }
    }
}
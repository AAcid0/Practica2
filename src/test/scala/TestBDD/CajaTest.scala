import org.scalatest.GivenWhenThen
import org.scalatest.FeatureSpec
import Alimentos._
import InterfazUsuario._
import InsumosPlasticos._

class CajaTest extends FeatureSpec with GivenWhenThen
{
    var tipoGrande : TipoTamano = new TipoTamano
    var tipoMediano : TipoTamano = new TipoTamano
    var tipoPequeno : TipoTamano = new TipoTamano
    var caja : Caja = new Caja
    var referenciaPapas : String = _
    var referenciaHamburguesa : String = _
    var referenciaBebida : String = _

    tipoGrande.descripcion = "Grande"    
    tipoMediano.descripcion = "Mediana"    
    tipoPequeno.descripcion = "Pequeño"    

    feature("Crear Alimento")
    {
        scenario("Hamburguesa Ranchera con valor de $20000")
        {
            Given("Dada una hamburguesa ranchera con valor de $20000")
            var hamburguesaRanchera : Hamburguesa = new Hamburguesa(tipoMediano, "Ranchera", 20000)
            referenciaHamburguesa = hamburguesaRanchera.referencia

            When("Se agrega la hamburguesa")
            caja.agregarAlimentoCatalogo(hamburguesaRanchera)

            Then("El catálogo no se encuentra vacío")
            var catalogo : List[Alimento] = caja.mostrarCatalogo()
            assert(catalogo.length > 0)
        }

        scenario("Bebida CocaCola con valor de 5000 grande")
        {
            Given("Dada una bebida CocaCola con valor de 5000 grande")
            var cocaCola : Bebida = new Bebida(tipoGrande, "CocaCola", 5000)
            referenciaBebida = cocaCola.referencia

            When("Se agrega la bebida al catálogo de la caja")
            caja.agregarAlimentoCatalogo(cocaCola)

            Then("El catálogo tiene 2 elementos")
            assert(caja.mostrarCatalogo().length == 2)
        }

        scenario("Crear unas papas pequeñas con valor de 4000 con queso")
        {
            Given("Dadas unas papas pequeñas con valor de 4000 con queso")
            var papasPequenas : Papas = new Papas(tipoPequeno, "Papas Fritas Pequeñas", 4000, true)
            referenciaPapas = papasPequenas.referencia

            When("Se agrega las papas a al catálogo de la caja")
            caja.agregarAlimentoCatalogo(papasPequenas)

            Then("La referencia de las papas se encuentra en el catálogo")
            var catalogo : List[Alimento] = caja.mostrarCatalogo()
            var papasEncontradas : List[Alimento] = catalogo.filter(a => a.referencia == papasPequenas.referencia)
            assert(papasEncontradas.length == 1)
        }
    }

    feature("Crear Pedido")
    {
        scenario("Agregar al pedido una hamburguesa y unas papas")
        {
            Given("Dados dos elementos de catálogo")
            var catalogo : List[Alimento] = caja.mostrarCatalogo()
            var hamburguesa : Alimento = catalogo.filter(h => h.referencia == referenciaHamburguesa).head
            var papas : Alimento = catalogo.filter(h => h.referencia == referenciaPapas).head

            When("Se agregan los elementos al pedido")
            caja.agregarProductoPedido(hamburguesa)
            caja.agregarProductoPedido(papas)

            Then("El costo del pedido será igual a 24000")
            var costo : Double = caja.comprar()
            assert(costo == 24480)
        }

        scenario("Agregar al pedido una hamburguesa, dos bebidas y todo para llevar")
        {
            Given("Dados dos elementos de catálogo")
            var catalogo : List[Alimento] = caja.mostrarCatalogo()
            var hamburguesa : Alimento = catalogo.filter(h => h.referencia == referenciaHamburguesa).head
            var bebida : Alimento = catalogo.filter(h => h.referencia == referenciaBebida).head

            When("Se agregan los elementos al pedido")
            var tapaCocaCola : TapaBebida = new TapaBebida
            var empaqueHamburguesa : EmpaquePlastico = new EmpaquePlastico
            var hamburguesaFinal = hamburguesa.asInstanceOf[Hamburguesa]
            hamburguesaFinal.esAgrandable = true
            caja.agregarProductoPedido(hamburguesaFinal)
            caja.agregarProductoPedido(empaqueHamburguesa)
            caja.agregarProductoPedido(bebida)
            caja.agregarProductoPedido(tapaCocaCola)
            caja.agregarProductoPedido(bebida)
            caja.agregarProductoPedido(tapaCocaCola)

            Then("El costo será de 30900")
            var costo : Double = caja.comprar()
            assert(costo == 30900)
        }

    }

    
}

package InterfazUsuario

import Adicionales._
import InsumosPlasticos._
import Alimentos._

class Caja
{
    /*Atributos*/
    private var _pedido : List[InsumoCocina] = List()
    private var _catalogo : List[Alimento] = List()
    private var _catalogoSalsas : List[Salsa] = List()
    private var _tiposTamano : List[TipoTamano] = List()

    /*Métodos*/
    def agregarProductoPedido(producto : InsumoCocina) : Unit =
    {
        _pedido = producto :: _pedido
    }

    def agregarAlimentoCatalogo(alimento : Alimento) : Unit =
    {
        _catalogo = alimento :: _catalogo
    }

    def agregarSalsaCatalogo(salsa : Salsa) : Unit =
    {
        _catalogoSalsas = salsa :: _catalogoSalsas
    }

    def agregarTamano(tamano : TipoTamano) : Unit =
    {
        _tiposTamano = tamano :: _tiposTamano
    }

    def comprar() : Double =
    {
        var costoTotal : Double = 0
        _pedido.foreach{p =>
        {
            costoTotal += p.calcularCosto()
        }}
        _pedido = List()
        return costoTotal
    }

    def mostrarCatalogo() : List[Alimento] = _catalogo
    def mostrarSalsas() : List[Salsa] = _catalogoSalsas
    def mostrarTamanos() : List[TipoTamano] = _tiposTamano
}
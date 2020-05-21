package InterfazUsuario

import scala.io._
import Alimentos._
import InsumosPlasticos._
import Adicionales._
import scala.util._

object InterfazUsuario extends App
{
    var cerrarTienda : Boolean = false
    private var caja : Caja = new Caja
    private var tipoGrande : TipoTamano = new TipoTamano()
    tipoGrande.descripcion = "Grande"
    caja.agregarTamano(tipoGrande)

    while(!cerrarTienda)
    {
        println("\nBienvenido a la configuración de comidas rápidas")
        println("Sus opciones son: \n 1->Crear Alimento \n 2-> MostrarCatalogo \n 3-> Agregar una salsa \n 4-> Agregar un tamaño\n 5-> Mostar tamaños \n 6-> Mostar salsas\n 7-> Salir")
        println("Escoja una opción: ")
        var opcion : Int = StdIn.readInt()
        if (opcion == 1)
        {
            var resultadoOperacion = crearAlimentoCatalogo()
            resultadoOperacion match
            {
                case Success(s) => println("Producto Creado Correctamente")
                case Failure(f) => println(f)
            }    
        }
        if (opcion == 2)
        {
            var probarCatalogo = comprobarCatalogo()
            probarCatalogo match
            {
                case Success(s) => mostrarComidas()
                case Failure(f) => println(f) 
            }
            
        }
        if (opcion == 3)
        {
            var agrSalsa = funAgregarSalsa()
            agrSalsa match
            {
                case Success(s) => caja.mostrarSalsas().foreach(p => println(p.nombre + " " + p.referencia))
                case Failure(f) => println(f) 
            }
            
        }
        if (opcion == 4)
        {
            var agrTamano = funAgregarTamano()
            agrTamano match
            {
                case Success(s) => {
                    var lisTamanos : List[TipoTamano] = caja.mostrarTamanos() 
                    println("Tamaños actuales: ")
                    lisTamanos.foreach{ t => 
                    {
                        println("Referencia : " + t.idTipoTamano + " Descripción: " + t.descripcion)
                    }}
                }
                case Failure(f) => println(f) 
            }
            
        }
        if (opcion == 5)
        {
            if(caja.mostrarTamanos.isEmpty == true)
            {
                println("La lista de tamaños se encuentra vacía")
            }
            else
            {
                shoTamano()
            }
        }
        if (opcion == 6)
        {
            if(caja.mostrarSalsas.isEmpty == true)
            {
                println("La lista de salsas se encuentra vacía")
            }
            else
            {
                shoSalsas()
            }
        }
        if (opcion == 7)
        {
            println("Cerrando sistema.")
            cerrarTienda = true
        }
    }

    def comprobarCatalogo() : Try[Unit] =
    {
        return Try{
            if(caja.mostrarCatalogo.isEmpty)
            {
                throw new Exception("El catalogo está vacío\n")
            }
        }
    }
    def mostrarComidas() : Unit =
    {
        var listaHamburguesas = caja.mostrarCatalogo.filter(x => x.tipoAlimento == "Hamburguesa").asInstanceOf[List[Hamburguesa]]
        var listaPapas = caja.mostrarCatalogo.filter(x => x.tipoAlimento == "Papas").asInstanceOf[List[Papas]]
        var listaBebidas = caja.mostrarCatalogo.filter(x => x.tipoAlimento == "Bebida").asInstanceOf[List[Bebida]]
        if(listaHamburguesas.nonEmpty)
        {
            println("\nHamburguesas\n==================\n")
            listaHamburguesas.foreach(p => println(p.descripcion + " " + p._costo + " " + p.referencia))
        }
        if(listaPapas.nonEmpty)
        {
            println("\nPapas\n==================\n")
            listaPapas.foreach(p => println(p.descripcion + " " + p._costo + " " + p.referencia))
        }
        if(listaBebidas.nonEmpty)
        {
            println("\nBebidas\n==================\n")
            listaBebidas.foreach(p => println(p.descripcion + " " + p._costo + " " + p.referencia))
        }
    }
    def crearAlimentoCatalogo() : Try[Unit] = 
    {
        return Try{
            println("Qué alimento desea crear? Hamburguesa, Bebida o Papas");
            var opcionComida : String = StdIn.readLine()
            var claseComida = Class.forName("Alimentos." + opcionComida)
            var instanciaComida = claseComida.newInstance();
            var comida = instanciaComida.asInstanceOf[Alimento]
            println("Coloque el nombre de su " + opcionComida)
            var nombre : String = StdIn.readLine()
            println("Coloque el precio de su " + opcionComida)
            var precio : Double = StdIn.readDouble()
            comida.descripcion = nombre
            comida.setCosto(precio)
            var tipoTamano : Option[TipoTamano] = elegirTamano()
            if (tipoTamano.isEmpty)
            {
                throw new Exception("Lista de tamaños Vacía")
            }
            comida.tamano = tipoTamano.get
            caja.agregarAlimentoCatalogo(comida)
        }
    }

    def elegirTamano() : Option[TipoTamano] = 
    {
        var tamanos : List[TipoTamano] = caja.mostrarTamanos()
        println("Tamaños actuales: ")
        tamanos.foreach{ t => 
        {
            println("Referencia : " + t.idTipoTamano + " Descripción: " + t.descripcion)
        }}
        println("Escriba la referecia de su tamaño: ")
        var referencia : String = StdIn.readLine()
        var tipoTamanoElejido : Option[TipoTamano] = tamanos.filter(t => t.idTipoTamano == referencia).headOption
        return tipoTamanoElejido
    }

    def funAgregarSalsa() : Try[Unit] = 
    {
        return Try{
            println("Digite el nombre de la salsa que desea crear")
            var nomSalsa : String = StdIn.readLine()
            var nuevaSalsa : Salsa = new Salsa(nomSalsa)
            caja.agregarSalsaCatalogo(nuevaSalsa)
            println("Salsa Agregada exitosamente")
        }
    }

    def funAgregarTamano() : Try[Unit] =
    {
        return Try{
            println("Ingrese la descripción del tamaño que desea agreagar")
            var desc : String = StdIn.readLine()
            var nuevoTam : TipoTamano = new TipoTamano()
            nuevoTam.descripcion = desc
            caja.agregarTamano(nuevoTam)
            println("Tamaño agregado exitosamente")
        }
    }

    def shoTamano() : Unit =
    {
        var tamanos : List[TipoTamano] = caja.mostrarTamanos()
        println("Tamaños actuales: ")
        tamanos.foreach{ t => 
        {
            println("Referencia : " + t.idTipoTamano + "-  Descripción: " + t.descripcion)
        }}
    }

    def shoSalsas() : Unit =
    {
        var sauces : List[Salsa] = caja.mostrarSalsas()
        println("Salsas Disponibles: ")
        sauces.foreach{ t => 
        {
            println("Nombre : " + t.nombre + " -  Referencia: " + t.referencia)
        }}
    }



}
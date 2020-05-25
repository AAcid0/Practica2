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
        println("\nBienvenido a fastfood.io, comida rápida")
        println("Ingresa la opción que se adapte a tu perfil")
        println("1-> Soy cliente.")
        println("2-> Soy vendedor.")
        println("3-> Salir.")
        println("Tu opción es: ")
        var perfil : Int = StdIn.readInt()
        perfil match
        {
            case 1 => {
                var sesionCliente : Boolean = true
                while(sesionCliente)
                {
                    println("\nBienvenido a fastfood.io, comida rápida")
                    println("Tus opciones son:")
                    println("1-> Ver Carta.")
                    println("2-> Comprar.")
                    println("3-> Volver.")
                    println("Su elección: ")
                    var opcion : Int = StdIn.readInt()
                    opcion match
                    {
                        case 1 => {
                            var probarCatalogo = comprobarCatalogo()
                            probarCatalogo match
                            {
                                case Success(s) => mostrarComidas()
                                case Failure(f) => println(f) 
                            }
                        }
                        case 2 => {

                            var compraFin : List[Alimento] = comprar()
                            var costoTotal : Double = 0.0

                            for(i <- compraFin)
                            {
                                println(i.tipoAlimento + " " + i.descripcion + " " + i.calcularCosto() + " " + i.referencia)
                                costoTotal += i.calcularCosto()
                            }
                            println("El costo de la compra es de: " + costoTotal)

                        }
                        case 3 => {
                            sesionCliente = false
                        }
                    }
                }               
            }
            case 2 => {
                var sesionVendedor : Boolean = true
                while(sesionVendedor)
                {
                    println("\nPanel de Control - fastfood.io")
                    println("Tus opciones son:")
                    println("1-> Agregar alimento nuevo a la carta.\n2-> Agregar nueva salsa disponible.\n3-> Agregar nueva presentacion disponible.")
                    println("4-> Mostrar presentaciones disponibles.\n5-> Mostrar salsas disponibles.\n6-> Mostrar catalogo de comidas.")
                    println("7-> Volver.")
                    println("Su elección: ")
                    var opcion : Int = StdIn.readInt()
                    opcion match
                    {
                        case 1=> {
                            var resultadoOperacion = crearAlimentoCatalogo()
                            resultadoOperacion match
                            {
                                case Success(s) => println("Producto Creado Correctamente")
                                case Failure(f) => println(f)
                            }
                        }
                        case 2 => {
                            var agrSalsa = funAgregarSalsa()
                            agrSalsa match
                            {
                                case Success(s) => caja.mostrarSalsas().foreach(p => println(p.nombre + " " + p.referencia))
                                case Failure(f) => println(f) 
                            }
                        }
                        case 3 => {
                            var agrTamano = funAgregarTamano()
                            agrTamano match
                            {
                                case Success(s) => {
                                            var lisTamanos : List[TipoTamano] = caja.mostrarTamanos() 
                                            println("Tamaños actuales: ")
                                            lisTamanos.foreach{ t => {
                                                println("Referencia : " + t.idTipoTamano + " Descripción: " + t.descripcion)
                                            }}
                                        }
                                case Failure(f) => println(f) 
                            }
                        }
                        case 4 => {
                            if(caja.mostrarTamanos.isEmpty == true)
                            {
                                println("La lista de tamaños se encuentra vacía")
                            }
                            else
                            {
                                shoTamano()
                            }
                        }
                        case 5 => {
                            if(caja.mostrarSalsas.isEmpty == true)
                            {
                                println("La lista de salsas se encuentra vacía")
                            }
                            else
                            {
                                shoSalsas()
                            }
                        }
                        case 6 => {
                            var probarCatalogo = comprobarCatalogo()
                            probarCatalogo match
                            {
                                case Success(s) => mostrarComidas()
                                case Failure(f) => println(f) 
                            }
                        }
                        case 7 => {
                            sesionVendedor = false
                        }
                    }
                }
            }
            case 3 => {
                cerrarTienda = true
            }
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
        println("Escriba la referencia de su tamaño: ")
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

    def comprar() : List[Alimento] =
    {

        var compraTerminada : Boolean = false
        var compra : List[Alimento] = List()
        while(!compraTerminada)
        {
            println("\nBienvenido a fastfood.io, comida rápida")
            println("¿Qué desea comprar?")
            println("1-> Hamburguesa")
            println("2-> Bebida")
            println("3-> Papas")
            println("4- Finalizar compra")
            println("Ingrese su elección: ")
            var opcion : Int = StdIn.readInt()

            opcion match
            {
                case 1 =>{
                    var listaHamburguesas = caja.mostrarCatalogo.filter(x => x.tipoAlimento == "Hamburguesa").asInstanceOf[List[Hamburguesa]]
                    if(listaHamburguesas.nonEmpty)
                    {
                        println("\nHamburguesas\n==================\n")
                        listaHamburguesas.foreach(p => println(p.descripcion + " " + p._costo + " " + p.referencia))
                    }
                    println("¿Qué hamburguesa desea?")
                    var ham : String = StdIn.readLine()
                    if(listaHamburguesas.exists(y => y.descripcion == ham))
                    {
                        for( i <- listaHamburguesas)
                        {
                            if(i.descripcion == ham)
                            {
                                println("¿Desea agrandar su hamburguesa?(si/no)")
                                var op : String = StdIn.readLine()
                                
                                if(op == "si")
                                {
                                    i.esAgrandable = true
                                    compra = i :: compra
                                }
                                else if(op == "no")
                                {
                                    compra = i :: compra
                                }
                                println("¿Deseas adicionar alguna salsa? (si/no)")
                                var opSal : String = StdIn.readLine()
                                if(opSal == "si")
                                {
                                    shoSalsas()
                                    println("\nIngresa el nombre de tu salsa:")
                                    var opcSal : String = StdIn.readLine()
                                    println("Salsa agregada al pedido.")
                                }
                            }
                        }
                    }
                    else
                    {
                        println("Selección no disponible, por favor verifique la carta.")
                    }
                }
                case 2 =>{
                    var listaBebidas = caja.mostrarCatalogo.filter(x => x.tipoAlimento == "Bebida").asInstanceOf[List[Bebida]]
                    if(listaBebidas.nonEmpty)
                    {
                        println("\nBebidas\n==================\n")
                        listaBebidas.foreach(p => println(p.descripcion + " " + p._costo + " " + p.referencia))
                    }
                    println("\n¿Qué bebida desea?")
                    var beb : String = StdIn.readLine()
                    if(listaBebidas.exists(y => y.descripcion == beb))
                    {
                        for( i <- listaBebidas)
                        {
                            if(i.descripcion == beb)
                            {
                                println("¿Desea su bebida con hielo?(si/no)")
                                var op : String = StdIn.readLine()
                                
                                if(op == "si")
                                {
                                    i.conHielo = true
                                    compra = i :: compra
                                }
                                if(op == "no")
                                {
                                    compra = i :: compra
                                }
                            }
                        }
                    }
                    else{
                        println("Selección no disponible, por favor verifique la carta.")
                    }
                }
                case 3 =>{
                    var listaPapas = caja.mostrarCatalogo.filter(x => x.tipoAlimento == "Papas").asInstanceOf[List[Papas]]
                    if(listaPapas.nonEmpty)
                    {
                        println("\nPapas\n==================\n")
                        listaPapas.foreach(p => println(p.descripcion + " " + p._costo + " " + p.referencia))
                    }
        
                    println("\n¿Qué tipo de papas desea?")
                    var pap : String = StdIn.readLine()
                    if(listaPapas.exists(y => y.descripcion == pap))
                    {
                        for( i <- listaPapas)
                        {
                            if(i.descripcion == pap)
                            {
                                println("¿Desea sus papas con queso?(si/no)")
                                var op : String = StdIn.readLine()
                                
                                if(op == "si")
                                {
                                    i.conQueso = true
                                    compra = i :: compra
                                }
                                if(op == "no")
                                {
                                    compra = i :: compra
                                }
                            }
                        }
                    }
                    else
                    {
                        println("Selección no disponible, por favor verifique la carta.")
                    }
                }
                case 4 =>{
                    compraTerminada = true
                }
                case default => println("Por favor ingrese una opción válida.")
            }
        }

        return compra
        
    }

}
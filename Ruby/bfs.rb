# 2 Busqueda Generalizada
# Patrick Rengifo   09-10703
# Daniela Rodriguez 09-10735

# Recorrido BFS en un comportamiento

module Bfs
  # Recorrido BFS para encontrar un nodo que cumpla predicate
  def find(start, predicate)
    if !(predicate.is_a?(Proc))
      puts "No se ha pasado un predicado valido"
      return nil
    end
    # Conjunto de nodos visitados
    nodes = {start.value => false}
    
    # Creamos la pila de nodos
    items = [start]
    nodes[start.value] = true
    
    while item = items.delete_at(0)
      # Si es el nodo cumple con el predicado, lo retornamos.
      return item if predicate.call(item.value)
      
      # Sino buscamos sus adyacentes y los empilamos.
      item.each(1) do |x| 
        unless nodes[x.value] 
          nodes[x.value] = true 
          items.push(x)
        end 
      end
    end
    # Si no encontramos nada
    return nil
  end
  
  # Recorrido BFS que devuelve el camino desde la raiz hasta el nodo que cumpla
  # el predicate
  def path(start, predicate)
    if !(predicate.is_a?(Proc))
      puts "No se ha pasado un predicado valido"
      return nil
    end
    # Conjunto de nodos visitados
    nodes = {start.value => false}
    # Camino vacio
    path = []
    
    # Creamos la pila de nodos
    items = [start]
    nodes[start.value] = true
    
    while item = items.delete_at(0)
      path.push(item.value)
      # Si es el nodo cumple con el predicado, lo retornamos.
      return path if predicate.call(item.value)
      
      # Sino buscamos sus adyacentes y los empilamos.
      item.each(1) do |x| 
        unless nodes[x.value] 
          nodes[x.value] = true 
          items.push(x)
        end 
      end
    end
    # Si no encontramos nada
    return nil
  end
  
  # Recorrido BFS que recorre en su totalidad el espacio de busqueda ejecutando
  # el action en cada nodo y retornando la lista de nodos recorridos. Si se 
  # omite action, solo devuelve la lista de nodos recorridos.
  def walk(start, action)
    # Conjunto de nodos visitados
    nodes = {start.value => false}
    # Camino vacio
    path = []
    
    # Creamos la pila de nodos
    items = [start]
    nodes[start.value] = true
    
    while item = items.delete_at(0)
      if action.is_a?(Proc)
        path.push(action.call(item.value))
      else
        path.push(item.value)
      end
      
      
      # Sino buscamos sus adyacentes y los empilamos.
      item.each(1) do |x| 
        unless nodes[x.value] 
          nodes[x.value] = true 
          items.push(x)
        end 
      end
    end
    # Si no encontramos nada
    return path
  end
end

# Arboles y Grafos Explicitos

class BinTree
  include Bfs
  attr_accessor :value,    # Valor almacenado en el nodo
                :left,     # BinTree izquierdo
                :right     # BinTree derecho
  def initialize(v,l=nil,r=nil)
    @value = v
    @left  = l
    @right = r
  end
  def each(b)
    yield self
    yield @left unless @left.nil?
    yield @right unless @right.nil?
  end
end

class GraphNode
  include Bfs
  attr_accessor :value,     # Valor alamacenado en el nodo
                :children   # Arreglo de sucesores GraphNode
  def initialize(v,c=nil)
    @value    = v
    @children = c
  end
  def each(b)
    yield self
    @children.each {|x| yield x } unless @children.nil?
  end
end

# Arboles Implicitos

def LCR
  include Bfs
  attr_reader :value
#   def initialize(?) # Indique los argumentos
    # Su codigo aqui
#   end
  def each(p)
    # Su codigo aqui
  end
  def solve       
    # Su codigo aqui
  end
end

b = BinTree.new(5,BinTree.new(4, BinTree.new(3)))
# b.each(1) {|x| puts x.value}
res = b.find(b,lambda {|x| x<4})
puts "find bintree"
puts res.value unless res.nil?
puts "path bintree"
puts b.path(b,lambda {|x| x==3})
puts "walk bintree"
puts b.walk(b,lambda{|x| x*2})
puts "----"
puts b.walk(b,2)

g = GraphNode.new(4, [GraphNode.new(3), GraphNode.new(2)])
# g.each(1) {|x| puts x.value}
res = g.find(g,lambda {|x| x>8})
puts "find graph"
puts res.value unless res.nil?
puts "path graph"
puts g.path(g,lambda {|x| x==3})
puts "walk graph"
puts g.walk(g,3)
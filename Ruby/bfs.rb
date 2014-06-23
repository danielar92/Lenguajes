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
      item.each do |x| 
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
      item.each do |x| 
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
      item.each do |x| 
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
  # Metodo propio de each
  def each
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
  # Metodo propio de each
  def each
    yield self
    @children.each {|x| yield x } unless @children.nil?
  end
end

# Arboles Implicitos

class LCR
  include Bfs
  attr_reader :value
  # Un estado del problema se modela con un hash con keys :where (donde esta
  # el bote), :left (que hay en la orilla izquierda, y :right (que hay en la
  # orilla derecha). Con los valores :repollo, :lobo, :cabra.
  def initialize(w,l,r)
    @value = {:where => w,
              :left => l,
              :right => r}
  end
  # Metodo privado para saber si un estado generado es valido para nuestro
  # problema a resolver. I.e. la cabra no puede estar con el lobo en la 
  # misma orilla sin el humano.
  def is_valid?
    if @value.has_value?([:cabra,:lobo]) || @value.has_value?([:cabra,:lobo]) ||
        @value.has_value?([:lobo,:cabra]) || @value.has_value?([:lobo,:cabra])
      return false
    elsif @value.has_value?([:cabra,:repollo]) || @value.has_value?([:cabra,:repollo]) ||
        @value.has_value?([:repollo,:cabra]) || @value.has_value?([:repollo,:cabra])
      return false
    else
      return true
    end
  end
  # Metodo propio de each.
  def each
    if is_valid?
      puts "woohooo"
    else
      puts "damn women"
    end
  end
  # Dado el estado final del problema LCR, se busca el camino que proporcione
  # la solucion al mismo.
  def solve
    final = lambda{|x| x.value[:right].has_value?([:cabra,:lobo,:repollo]) ||
                    x.value[:right].has_value?([:lobo,:repollo,:cabra]) ||
                    x.value[:right].has_value?([:cabra,:repollo,:lobo])}
    result = self.path(self, final)
    if result.nil?
      puts "El problema no tiene solucion"
    else
      puts "El resultado al problema es"
      puts result
    end
  end
  private :is_valid?
end
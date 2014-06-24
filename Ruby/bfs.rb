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
  # Metodo para saber si un estado generado es valido para nuestro
  # problema a resolver. I.e. la cabra no puede estar con el lobo en la 
  # misma orilla sin el humano. Ademas mantiene que solo haya un elemento
  # en cada orilla.
  def is_valid?
    if @value.has_value?([:cabra,:lobo]) || 
       @value.has_value?([:cabra,:lobo]) ||
       @value.has_value?([:lobo,:cabra]) ||
       @value.has_value?([:lobo,:cabra]) ||
       @value.has_value?([:cabra,:repollo]) || 
       @value.has_value?([:cabra,:repollo]) ||
       @value.has_value?([:repollo,:cabra]) || 
       @value.has_value?([:repollo,:cabra]) ||
       (@value[:left].include?(:lobo) && @value[:right].include?(:lobo)) ||
       (@value[:left].include?(:cabra) && @value[:right].include?(:cabra)) ||
       (@value[:left].include?(:repollo) && @value[:right].include?(:repollo))
      return false
    else
      return true
    end
  end
  # Metodo propio de each.
  def each
    # Self es valido
    if is_valid?
      # Movemos el barco a la orilla contraria solo o con algo
      if @value[:where] == :izquierda
        # Barco solo
        barco = LCR.new(:derecha, @value[:left], @value[:right])
        yield barco unless !(barco.is_valid?)
        # Barco con algo
        @value[:left].each do |x| barco = LCR.new(:derecha, 
                                                  @value[:left].delete(x),
                                                  @value[:right].push(x))
          yield barco unless !(barco.is_valid?)
        end
      else 
        # Barco solo
        barco = LCR.new(:izquierda, @value[:left], @value[:right])
        yield barco unless !(barco.is_valid?)
        # Barco con algo
        @value[:right].each do |x| barco = LCR.new(:izquierda, 
                                                   @value[:left].push(x),
                                                   @value[:right].delete(x))
          yield barco unless !(barco.is_valid?)
        end
      end
    end
  end
  # Dado el estado final del problema LCR, se busca el camino que proporcione
  # la solucion al mismo.
  def solve
    final = lambda{|x| x[:right] == [:cabra,:lobo,:repollo] ||
                    x[:right] == [:lobo,:repollo,:cabra] ||
                    x[:right] ==[:cabra,:repollo,:lobo]}
    result = self.path(self, final)
    if result.nil?
      puts "El problema no tiene solucion"
    else
      puts "El resultado al problema es"
      puts result
    end
  end
end

l = LCR.new(:izquierda,[:lobo,:repollo,:cabra],[])
l.solve
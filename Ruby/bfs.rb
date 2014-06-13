# 2 Busqueda Generalizada
# Patrick Rengifo   09-10703
# Daniela Rodriguez 09-10735

# Recorrido BFS en un comportamiento

module Bfs
  def find(start, predicate)
  end
  def path(start, predicate)
  end
  def walk(start, predicate)
  end
end

# Arboles y Grafos Explicitos

class BinTree
  include Bfs
  attr_accessor :value, # Valor almacenado en el nodo
                :left, # BinTree izquierdo
                :right # BinTree derecho
  def initialize(v,l,r)
    @value = v
    @left  = l
    @right = r
  end
  def each(b)
    @left.each {|binTree| yield binTree } unless @left.nil?
    yield self
    @right.each {|binTree| yield binTree } unless @right.nil?
  end
end

class GraphNode
  include Bfs
  attr_accessor :value, # Valor alamacenado en el nodo
                :children # Arreglo de sucesores GraphNode
  def initialize(v,c)
    @value    = v
    @children = c
  end
  def each(b)
    @children.each {|graphNode| yield graphNode } unless @children.nil?
  end
end

# Arboles Implicitos

def LCR
  include Bfs
  attr_reader :value
  def initialize(?) # Indique los argumentos
    # Su codigo aqui
  end
  def each(p)
    # Su codigo aqui
  end
  def solve       
    # Su codigo aqui
  end
end
# 2 Busqueda Generalizada
# Patrick Rengifo   09-10703
# Daniela Rodriguez 09-10735

# Recorrido BFS en un comportamiento

module Bfs
  def find(start, predicate)
#   def bfs(start_node_key, find_node)
#     keys_explored = {start.each(1)}
#     keys_explored = Hash.new.tap do |h|
#       @nodes.keys.each do |key|
#         h[key] = false
#       end
#     end
#  
#     items = [start]
#     keys_explored[start] = true
#  
#     while item = items.delete_at(0)
#       return @nodes[find_node] if item == find_node
#  
#       @edges[item].each do |head, value|
#         unless keys_explored[head]
#           keys_explored[head] = true
#           items.push head
#         end
#       end
#     end
#     
#     return nil
  end
  def path(start, predicate)
  end
  def walk(start, predicate)
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
    @left.each(b) {|x| yield x } unless @left.nil?
    yield self
    @right.each(b) {|x| yield x } unless @right.nil?
  end
end

b = BinTree.new(5,BinTree.new(4))
b.each(1) {|x| puts x.value}

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

g = GraphNode.new(4, [GraphNode.new(3), GraphNode.new(2)])
g.each(1) {|x| puts x.value}

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
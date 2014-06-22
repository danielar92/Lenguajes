# -*- coding: utf-8 -*-
# Daniela Rodríguez 09-10735
# Patrick Rengifo 09-10703
# Juego de manos

class Movement

  class << self
    attr_accessor :symbol
  end

  def to_s
    puts self.class
  end


  def score(m)
    if (self.class==m.class) then [0,0]
    elsif (self.class==Rock) and (m.class==Scissors) then [1,0]
    elsif (self.class==Scissors) and (m.class==Paper) then [1,0]
    elsif (self.class==Paper) and (m.class==Rock) then [1,0]
    else [0,1]
    end

  end

  def self.from_symbol(symbol)
    case symbol
    when :Rock
      Rock.new
    when :Scissors
      Scissors.new
    when :Paper
      Paper.new
    else
      raise ArgumentError, "Simbolo desconocido para movimiento."
    end
  end
end

class Rock < Movement
  @symbol = :Rock
end

class Paper < Movement
  @symbol = :Paper
end

class Scissors < Movement
  @symbol = :Scissors
end


class Strategy
  @@seed = 42
  def initialize
    @prng = Random.new @@seed
  end

   def next(ms)
     raise NotImplementedError
   end

   #Si es Uniform o Biased imprimimos su entrada inicial
   def to_s
     puts self.class
     if self.class == Uniform or self.class == Biased
       puts @mhash
     end
   end

   def reset
   end
end


class Biased < Strategy
  def initialize(mhash)
    super()
    if mhash.empty? then
      raise ArgumentError, "Movimientos vacios"
    end
    @mhash = mhash
    build_sum_hash
  end

  # Construye el hash que contiene el movimiento posible y su probabilidad asociadad
  def build_sum_hash
    @mhash_sum = {}
    @s = @mhash.values.reduce(:+)
    initial = 0
    @mhash.each_pair do |k, v|
      initial += v
      @mhash_sum[k] = initial
    end
  end

  #Selecciona el próximo movimiento a jugar de la "torta" construida en build_sum_hash
  def next(m)
    rng = @prng.rand(1..@s)
    nextSymbol = @mhash_sum.select { |k, v|  rng <= v }.sort_by { |k, v| v }
    Movement.from_symbol nextSymbol.first.first
  end
end

#Uniform es un Biased con probabilidad de 1 para cada movimiento
class Uniform < Biased
  def initialize(m)
    if m.empty? then
      raise ArgumentError, "Movimientos invalidos"
    end
    super Hash[m.uniq.collect{ |v| [v, 1] }]
  end
end

# Smart es un Biased inicializado en 0.
class Smart < Biased
  def initialize
    super ({:Scissors => 0, :Rock => 0, :Paper => 0})
  end
  # A medida que va jugando el oponente se va sumando al hash de jugadas
  def add_movement(m)
    unless m.nil?
      @mhash[m.class.symbol] += 1
      build_sum_hash
    end
  end

  # creamos el movimiento que gane al del oponente
  def get_winning(m)
    case m
    when Rock
      Paper.new
    when Paper
      Scissors.new
    when Scissors
      Rock.new
    end
  end

  def next(m)
    add_movement(m)
    if @s == 0
      next_movement = Movement.from_symbol @mhash.keys[@prng.rand(0..2)]
    else
      next_movement = get_winning(super m)
    end
    return next_movement
  end

  def reset
    @mhash = {:Scissors => 0, :Rock => 0, :Paper => 0}
    build_sum_hash
  end
end

#Juega el movimiento anterior del oponente.
class Mirror < Strategy
  def initialize(first_play)
    @first_play = first_play
    @prev_play = nil
  end


  def next(m)
    if @prev_play.nil? then
      @prev_play = m
      @first_play
    else
      save = @prev_play
      @prev_play = m
      save
    end
  end
  def reset
    @prev_play = nil
  end
end


class Match
  attr_accessor :state
  def initialize(shash)
    if shash.length != 2 then
      raise ArgumentError, "Tienes que pasar exactamente dos estrategias"
    end
    @state = {}
    shash.each_pair do |k, s|
      @state[k] = {
        :strategy => s,
      }
    end
    restart
  end


  def restart
    @state.each_pair do |k, s|
      s[:strategy].reset
      @state[k] = {
        :strategy => s[:strategy],
        :last_play => nil,
        :win_count => 0
      }
    end
    @game_count = 0
  end

  #retorna el otro jugador.
  def other_player(player)
    @state.keys.select { |k| k != player }.first
  end

  # construye el diccionario con el resultado del juego
  def build_result
    result = {:Rounds => @game_count}
    @state.each_key do |k|
      result[k] = @state[k][:win_count]
    end
    return result
  end

  #Se juegan n número de partidas
  def rounds(n)
    (1..n).each do |round|
      next_plays = @state.keys.map do |player|
        player_state = @state[player]
        [player, player_state[:strategy].next(player_state[:last_play])]
      end


      score = next_plays[0][1].score(next_plays[1][1])

      # zippeamos el movimiento con el jugador que lo realizó
      score.zip(next_plays).each do |tuple|
        player_score = tuple[0]
        who_played = tuple[1][0]
        movement = tuple[1][1]
        @state[other_player(who_played)][:last_play] = movement
        @state[who_played][:win_count] += player_score
      end
      @game_count += 1
    end
    return build_result
  end

  # retorna si alguien ya ganó mas de n veces
  def someone_won?(n)
    return @state.values.map { |ps| ps[:win_count]  >= n }.any?
  end

  def at_least_to(n)
    return @state.values.map { |ps| n-ps[:win_count] }.min
  end

  # Se juega hasta que alguien tenga n rondas ganadas.
  def upto(n)
    until someone_won?(n)
      last = rounds(at_least_to(n))
    end
    return last
  end
end

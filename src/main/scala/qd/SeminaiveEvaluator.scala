package qd

case class SeminaiveEvaluator(program: Program) extends Evaluator(program) {

  override def apply(edb: Config): Config = {
    val config = Config(allRelations.map(relation => relation -> edb.getOrElse(relation, Instance(relation))).toMap)
    var configPair = ConfigPair(config, config)
    while (configPair.delta.numTuples > 0) {
      val newPair = immediateConsequence(configPair)
      assert(newPair.config.numTuples >= configPair.config.numTuples)
      configPair = newPair
    }
    configPair.config
  }

  case class ConfigPair(config: Config, delta: Config)

  def immediateConsequence(configPair: ConfigPair): ConfigPair = {
    var newConfig = configPair.config
    var newDelta = Config(allRelations.map(schema => schema -> Instance(schema)).toMap)
    for (rule <- rules) {
      val relation = rule.head.relation
      val rulePair = immediateConsequence(rule, configPair)
      newConfig = newConfig + (relation -> (newConfig(relation) ++ rulePair.instance))
      newDelta = newDelta + (relation -> (newDelta(relation) ++ rulePair.delta))
    }
    ConfigPair(newConfig, newDelta)
  }

  case class InstancePair(instance: Instance, delta: Instance) {
    require(instance.relation == delta.relation)
    val relation: Relation = instance.relation
    def ++(that: InstancePair): InstancePair = InstancePair(instance ++ that.instance, delta ++ that.delta)
  }

  def immediateConsequence(rule: Rule, configPair: ConfigPair): InstancePair = {
    val relation = rule.head.relation
    val oldInstance = configPair.config(relation)

    var newInstance = oldInstance
    var newDelta = Instance(relation)
    for (literal <- rule.body) {
      val literalDelta = immediateConsequenceDelta(rule, literal, configPair)
      newInstance = newInstance ++ literalDelta
      newDelta = newDelta ++ literalDelta
    }

    InstancePair(newInstance, newDelta -- oldInstance)
  }

  def immediateConsequenceDelta(rule: Rule, literal: Literal, configPair: ConfigPair): Instance = {
    require(rule.body.contains(literal))

    var bodyVals = Set(Valuation())
    bodyVals = extend(literal, configPair.delta, bodyVals)
    for (literal <- rule.body) {
      bodyVals = extend(literal, configPair.config, bodyVals)
    }
    val newTuples = bodyVals.flatMap(rule.head.concretize)

    val newInstance = configPair.config(rule.head.relation) ++ newTuples
    newInstance
  }

  def extend(literal: Literal, config: Config, bodyVals: Set[Valuation]): Set[Valuation] = {
    for (valuation <- bodyVals;
         tuple <- config(literal.relation).tuples;
         newValuation <- extend(literal, tuple, valuation))
      yield newValuation
  }

  def extend(literal: Literal, tuple: DTuple, valuation: Valuation): Option[Valuation] = {
    var ans = valuation
    for ((par, field) <- literal.parameters.zip(tuple.fields)) {
      par match {
        case v @ Variable(_, _) =>
          if (!ans.contains(v)) ans = ans + (v -> field)
          else if (ans(v) != field) return None
        case Constant(c, _) => if (c != field) return None
      }
    }
    Some(ans)
  }

}

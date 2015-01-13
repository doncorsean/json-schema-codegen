import argonaut.Argonaut._
import argonaut._
import org.scalatest.{FlatSpec, Matchers}

object Codecs {

  import additional.properties.json.{AdditionalPropertiesJson, AdditionalPropertiesJsonAdditional}

  private def _AdditionalPropertiesJsonCodecSimple: CodecJson[AdditionalPropertiesJson] = casecodec2(AdditionalPropertiesJson.apply, AdditionalPropertiesJson.unapply)("b", "_additional")

  implicit def AdditionalPropertiesJsonCodec: CodecJson[AdditionalPropertiesJson] = CodecJson.derived(EncodeJson {
    v =>
      val j = _AdditionalPropertiesJsonCodecSimple.encode(v)
      val nj = j.field("_additional").fold(j)(a => j.deepmerge(a))
      nj.hcursor.downField("_additional").deleteGoParent.focus.getOrElse(nj)
  }, DecodeJson {
    c =>
      val md: DecodeJson[Option[Map[String, AdditionalPropertiesJsonAdditional]]] = implicitly
      val od: DecodeJson[AdditionalPropertiesJson] = _AdditionalPropertiesJsonCodecSimple
      for {
        o <- od.decode(c)
        withoutProps = List("b").foldLeft(c)((c, f) => c.downField(f).deleteGoParent.hcursor.getOrElse(c))
        m <- md.decode(withoutProps)
      } yield o.copy(_additional = m)
  })

  implicit def AdditionalPropertiesJsonAdditionalCodec: CodecJson[AdditionalPropertiesJsonAdditional] = casecodec1(AdditionalPropertiesJsonAdditional.apply, AdditionalPropertiesJsonAdditional.unapply)("a")

}


class ExamplesTest extends FlatSpec with Matchers {


  "AdditionalPropertiesJson" should "encode and decode" in {
    import additional.properties.json.Codecs._
    test(additional.properties.json.AdditionalPropertiesJson("bvalue", Some(Map("p1" -> additional.properties.json.AdditionalPropertiesJsonAdditional(1)))))
  }
  "AdditionalPropertiesOnlyJson" should "encode and decode" in {
    import additional.properties.only.json.Codecs._
    test(additional.properties.only.json.AdditionalPropertiesOnlyJson(Some(Map("p1" -> additional.properties.only.json.AdditionalPropertiesOnlyJsonAdditional(1)))))
  }


  def test[T: CodecJson](value: T) = {
    val json = value.asJson
    println(json)
    json.jdecode[T] shouldBe DecodeResult.ok(value)
  }
}

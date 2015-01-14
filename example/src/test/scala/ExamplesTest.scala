import argonaut.Argonaut._
import argonaut._
import org.scalatest.{FlatSpec, Matchers}


class ExamplesTest extends FlatSpec with Matchers {


  "AdditionalPropertiesJson" should "encode and decode" in {
    import additional.properties.Codecs._
    test(additional.properties.AdditionalProperties("bvalue", Some(Map("p1" -> additional.properties.AdditionalPropertiesAdditional(1)))))
  }

  "AdditionalPropertiesOnlyJson" should "encode and decode" in {
    import additional.properties.only.Codecs._
    test(additional.properties.only.AdditionalPropertiesOnly(Some(Map("p1" -> additional.properties.only.AdditionalPropertiesOnlyAdditional(1)))))
  }

  "EnumStrings" should "encode and decode" in {
    import strings.Strings
    import strings.Codecs._
    test(Strings.a)
    test(Strings.b)
  }
  "EnumIntegers" should "encode and decode" in {
    import integers.Integers
    import integers.Codecs._
    test(Integers.v10)
    test(Integers.v20)
  }


  def test[T: CodecJson](value: T) = {
    val json = value.asJson
    println(json)
    json.jdecode[T] shouldBe DecodeResult.ok(value)
  }
}

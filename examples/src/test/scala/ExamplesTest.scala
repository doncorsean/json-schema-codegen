import java.net.{Inet6Address, InetAddress, Inet4Address, URI}
import java.util.Date

import argonaut.Argonaut._
import argonaut._
import org.scalatest.{FlatSpec, Matchers}
import product.Product


class ExamplesTest extends FlatSpec with Matchers {


  "AdditionalPropertiesJson" should "encode and decode" in {
    import additional.PropertiesCodec._
    test(additional.Properties("bvalue", Some(Map("p1" -> additional.PropertiesAdditional(1)))))
  }

  "AdditionalPropertiesOnlyJson" should "encode and decode" in {
    import additional.properties.OnlyCodec._
    test(additional.properties.Only(Some(Map("p1" -> additional.properties.OnlyAdditional(1)))))
  }

  "EnumStrings" should "encode and decode" in {
    import strings.Strings
    import strings.StringsCodec._
    test(Strings.a)
    test(Strings.b)
  }

  "EnumIntegers" should "encode and decode" in {
    import integers.Integers
    import integers.IntegersCodec._
    test(Integers.v10)
    test(Integers.v20)
  }

  "Formats" should "encode and decode" in {
    import formats.Formats
    import formats.FormatsCodec._
    test(Formats(
      new URI("http://uri/address"),
      InetAddress.getByName("127.0.0.1").asInstanceOf[Inet4Address],
      InetAddress.getByName("FE80:0000:0000:0000:0202:B3FF:FE1E:8329").asInstanceOf[Inet6Address],
      new Date()
    ))
  }

  "Product" should "decode from string and encode to string" in {
    import product.ProductCodec._
    val js = """{"name":"Recharge Cards (5 PIN)","prices":[{"cost":0.0187,"currency":"USD","moq":200000}],"eid":"iso-card-5-pin","description":"<p>ISO card, 5 PINs, printed 4 colour front and back</p>\n<p>Every card option shown below meets Tier 1 operator quality standards, at a competitive pricing including freight to your country that’s always openly visible, with streamlined fulfillment and support included, creating what we believe is the best overall value at the lowest total cost of ownership in the industry.</p>\n<p>Material:        Cardboard 300 GSM, UV varnish both sides</p>\n<p>Scratch panel:   Silver/Black Ink with black overprint</p> \n<p>Individually plastic wrapped in chain of 50 cards</p>\n<p>Small boxes of 500 cards, Master Carton of 5000 cards</p>\n<p>Alternate names: Scratch cards, RCV, top-up cards</p>\n","properties":[{"name":"Overscratch Protection","options":[{"name":"No protection"},{"name":"Protective measures against over scratching","prices":[{"cost":0.0253,"currency":"USD","moq":200000},{"cost":0.021,"currency":"USD","moq":500000},{"cost":0.02,"currency":"USD","moq":1000000},{"cost":0.0188,"currency":"USD","moq":5000000,"leadtime":21},{"cost":0.0173,"currency":"USD","moq":10000000},{"cost":0.0171,"currency":"USD","moq":50000000,"leadtime":28}]}]},{"name":"Payment terms","options":[{"name":"Payment on shipment readiness"},{"name":"Net 30 (subject to approval)"}]},{"name":"Order Timing","options":[{"name":"Ship order when ready"},{"name":"Pre-order for shipment in 3 months"}]}],"client":"112","sample":{"price":{"cost":250,"currency":"USD"}},"category":"recharge_cards","leadtime":14,"imageUrl":["https://d2w2n7dk76p3lq.cloudfront.net/product_image/recharge_cards/iso-5pin.png"],"types":[{"name":"Recharge Cards (5 PIN)","prices":[{"cost":0.0187,"currency":"USD","moq":200000},{"cost":0.0175,"currency":"USD","moq":500000},{"cost":0.0162,"currency":"USD","moq":1000000},{"cost":0.0153,"currency":"USD","moq":5000000,"leadtime":21},{"cost":0.0138,"currency":"USD","moq":10000000,"leadtime":28},{"cost":0.0137,"currency":"USD","moq":50000000,"leadtime":28}]}],"presentation":1000}"""
    val po = js.decodeValidation[Product]
    println(po)
    po.isSuccess shouldBe true
    test(po.toOption.get)
  }


  def test[T: CodecJson](value: T) = {
    val json = value.asJson
    println(json)
    json.jdecode[T] shouldBe DecodeResult.ok(value)
  }
}

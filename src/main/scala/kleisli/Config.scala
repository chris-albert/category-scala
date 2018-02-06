package kleisli

import cats.data.Kleisli
import cats.instances.option._

object Config {


  case class DbConfig(url: String, pass: String)

  trait Db

  object Db {
    val fromDbConfig: Kleisli[Option, DbConfig, Db] = ???
  }

  case class ServiceConfig(addr: String, port: Int)

  trait Service

  object Service {
    val fromServiceConfig: Kleisli[Option, ServiceConfig, Service] = ???
  }

  case class AppConfig(dbConfig: DbConfig, serviceConfig: ServiceConfig)

  class App(db: Db, service: Service)

  def appFromConfig: Kleisli[Option, AppConfig, App] =
    for {
      db <- Db.fromDbConfig.local[AppConfig](_.dbConfig)
      sv <- Service.fromServiceConfig.local[AppConfig](_.serviceConfig)
    } yield new App(db, sv)

  type FeatureName = String
  type FeatureValue = Double

  //Something that can be stored
  case class Feature(name: FeatureName, value: FeatureValue)

  object Feature {
    def apply[A](extractor: FeatureExtractor[A], a: A): Feature =
      Feature(extractor.name, extractor.f(a))

    def get: Kleisli[Option, FeatureName, FeatureValue] = ???
  }

  //feature math

  case class FeatureExtractor[A](name: FeatureName, f: A => FeatureValue)


}

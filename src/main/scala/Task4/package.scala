import scala.concurrent.Future

package object Task4 {

  import cats.{Id, Monad}

  /**
    * Repository and Service implementation using tagless final pattern.
    * The idea is to make it easier to test our database layer, using Scala’s higher kinded types to abstract
    * the Future type constructor away from our traits under test.
    * Intro to tagless final: https://www.basementcrowd.com/2019/01/17/an-introduction-to-tagless-final-in-scala/.
    * The similar task example https://github.com/LvivScalaClub/cats-playground/blob/master/src/main/scala/BookRepository.scala
    */

  case class User(id: Long, username: String)

  case class IotDevice(id: Long, userId: Long, sn: String)

  // NOTE: This import bring into the scope implicits that allow you to call .map and .flatMap on the type F[_]
  // and also bring you typeclasses that know how to flatmap (Monad) and map (Functor) over your higher-kinded type.
  import cats.implicits._

  trait UserRepository[F[_]] {

    def registerUser(username: String): F[User]

    def getById(id: Long): F[Option[User]]

    def getByUsername(username: String): F[Option[User]]

  }

  trait IotDeviceRepository[F[_]] {

    def registerDevice(userId: Long, serialNumber: String): F[IotDevice]

    def getById(id: Long): F[Option[IotDevice]]

    def getBySn(sn: String): F[Option[IotDevice]]

    def getByUser(userId: Long): F[Seq[IotDevice]]

  }

  class UserRepositoryInMemoryId extends UserRepository[Id] {
    private var storage: Map[Long, User] = Map()
    private var incremental_id: Long = 0

    override def registerUser(username: String): Id[User] = {
      incremental_id += 1
      storage += (incremental_id -> User(incremental_id, username))
      User(incremental_id, username)
    }

    override def getById(id: Long): Id[Option[User]] = storage.get(id)

    override def getByUsername(username: String): Id[Option[User]] = {
      storage.filter(_._2.username == username).values.toList.headOption
    }

  }

  class UserRepositoryInMemoryFuture extends UserRepository[Future] {

    private var storage: Map[Long, User] = Map()
    private var incremental_id: Long = 0

    override def registerUser(username: String): Future[User] = Future.successful{
      incremental_id += 1
      storage += (incremental_id -> User(incremental_id, username))
      User(incremental_id, username)
    }

    override def getById(id: Long): Future[Option[User]] = Future.successful{
      storage.get(id)
    }

    override def getByUsername(username: String): Future[Option[User]] = Future.successful{
      storage.filter(_._2.username == username).values.toList.headOption
    }

  }

  class IotDeviceRepositoryInMemoryId extends IotDeviceRepository[Id] {

    private var storage: Map[Long, IotDevice] = Map()
    private var incremental_id: Long = 0

    override def registerDevice(userId: Long, serialNumber: String): Id[IotDevice] = {
      incremental_id += 1
      storage += (incremental_id -> IotDevice(incremental_id, userId, serialNumber))
      IotDevice(incremental_id, userId, serialNumber)
    }

    override def getById(id: Long): Id[Option[IotDevice]] = storage.get(id)

    override def getBySn(sn: String): Id[Option[IotDevice]] = {
      storage.filter(_._2.sn == sn).values.toList.headOption
    }

    override def getByUser(userId: Long): Id[Seq[IotDevice]] = {
      storage.filter(_._2.userId == userId).values.toSeq
    }

  }

  class IotDeviceRepositoryInMemoryFuture extends IotDeviceRepository[Future] {

    private var storage: Map[Long, IotDevice] = Map()
    private var incremental_id: Long = 0

    override def registerDevice(userId: Long, serialNumber: String): Future[IotDevice] = Future.successful{
      incremental_id += 1
      storage += (incremental_id -> IotDevice(incremental_id, userId, serialNumber))
      IotDevice(incremental_id, userId, serialNumber)
    }

    override def getById(id: Long): Future[Option[IotDevice]] = Future.successful{
      storage.get(id)
    }

    override def getBySn(sn: String): Future[Option[IotDevice]] = Future.successful{
      storage.filter(_._2.sn == sn).values.toList.headOption
    }

    override def getByUser(userId: Long): Future[Seq[IotDevice]] = Future.successful{
      storage.filter(_._2.userId == userId).values.toSeq
    }

  }

  class UserService[F[_]](repository: UserRepository[F])
                         (implicit monad: Monad[F]) {

    def registerUser(username: String): F[Either[String, User]] = {

      repository.getByUsername(username).flatMap({
        case Some(user) => monad.pure(Left(s"User $user already exists"))
        case None => repository.registerUser(username).map(Right(_))
      })
    }

    def getByUsername(username: String): F[Option[User]] = repository.getByUsername(username)

    def getById(id: Long): F[Option[User]] = repository.getById(id)
  }

  class IotDeviceService[F[_]](repository: IotDeviceRepository[F],
                               userRepository: UserRepository[F])
                              (implicit monad: Monad[F]) {

    def registerDevice(userId: Long, sn: String): F[Either[String, IotDevice]] = {
      repository.getBySn(sn).flatMap({
        case Some(_) => monad.pure(Left(s"Device Serial Number $sn already registered"))
        case None => repository.registerDevice(userId, sn).map(Right(_))
      })

    }

    def getById(id: Long): F[Option[IotDevice]] = repository.getById(id)

    def getBySn(sn: String): F[Option[IotDevice]] = repository.getBySn(sn)

    def getByUser(userId: Long): F[Seq[IotDevice]] = repository.getByUser(userId)

  }
}

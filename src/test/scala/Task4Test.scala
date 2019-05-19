import Task4._
import cats.Id
import cats.instances.future._
import org.scalatest.FunSuite

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.higherKinds


class Task4Test extends FunSuite {

  test("Task4.tagless") {
    val userRepository = new UserRepositoryInMemoryId()
    val iotDeviceRepository = new IotDeviceRepositoryInMemoryId()

    val userService = new UserService[Id](userRepository)
    val iotDeviceService = new IotDeviceService[Id](iotDeviceRepository, userRepository)

    val userName1 = "Ash Ketchum"
    val userName2 = "Brock"
    val userName3 = "Misty"

    userService.registerUser(userName1)
    userService.registerUser(userName2)
    userService.registerUser(userName3)


    val user1 = userService.getByUsername(userName1)
    val user2 = userService.getByUsername(userName2)
    val user3 = userService.getByUsername(userName3)

    assert(user1.get.id == 1)
    assert(user2.get.id == 2)
    assert(user3.get.id == 3)

    assert(user1.get.username == userName1)
    assert(user2.get.username == userName2)
    assert(user3.get.username == userName3)


    val deviceData1 = (user1.get.id, "Pikachu")
    val deviceData2 = (user2.get.id, "Onix")
    val deviceData3 = (user3.get.id, "Hypno's Naptime")

    (iotDeviceService.registerDevice _).tupled(deviceData1)
    (iotDeviceService.registerDevice _).tupled(deviceData2)
    (iotDeviceService.registerDevice _).tupled(deviceData3)

    assert(iotDeviceService.getByUser(user1.get.id).headOption.get.id == 1)
    assert(iotDeviceService.getByUser(user2.get.id).headOption.get.id == 2)
    assert(iotDeviceService.getByUser(user3.get.id).headOption.get.id == 3)

    assert(iotDeviceService.getByUser(user1.get.id).headOption.get.sn == deviceData1._2)
    assert(iotDeviceService.getByUser(user2.get.id).headOption.get.sn != deviceData1._2)
    assert(iotDeviceService.getByUser(user3.get.id).headOption.get.sn != deviceData1._2)

    assert(iotDeviceService.getByUser(user1.get.id).headOption.get.sn != deviceData2._2)
    assert(iotDeviceService.getByUser(user2.get.id).headOption.get.sn == deviceData2._2)
    assert(iotDeviceService.getByUser(user3.get.id).headOption.get.sn != deviceData2._2)

    assert(iotDeviceService.getByUser(user1.get.id).headOption.get.sn != deviceData3._2)
    assert(iotDeviceService.getByUser(user2.get.id).headOption.get.sn != deviceData3._2)
    assert(iotDeviceService.getByUser(user3.get.id).headOption.get.sn == deviceData3._2)

    assert(iotDeviceService.getByUser(user1.get.id).headOption.get.userId == user1.get.id)
    assert(iotDeviceService.getByUser(user1.get.id).headOption.get.userId != user2.get.id)

    assert(iotDeviceService.getByUser(user2.get.id).headOption.get.userId == user2.get.id)
    assert(iotDeviceService.getByUser(user2.get.id).headOption.get.userId != user3.get.id)

    assert(iotDeviceService.getByUser(user3.get.id).headOption.get.userId == user3.get.id)
    assert(iotDeviceService.getByUser(user3.get.id).headOption.get.userId != user1.get.id)

    assert(iotDeviceService.getBySn(deviceData1._2).headOption.get.userId == user1.get.id)
    assert(iotDeviceService.getBySn(deviceData2._2).headOption.get.userId == user2.get.id)
    assert(iotDeviceService.getBySn(deviceData3._2).headOption.get.userId == user3.get.id)

    assert(iotDeviceService.getBySn(deviceData1._2).headOption.get.sn == deviceData1._2)
    assert(iotDeviceService.getBySn(deviceData2._2).headOption.get.sn == deviceData2._2)
    assert(iotDeviceService.getBySn(deviceData3._2).headOption.get.sn == deviceData3._2)

  }

  test("Task4.taglessFuture") {
    def await[T](f: Future[T]) = Await.result(f, 2.seconds)


    val userRepository = new UserRepositoryInMemoryFuture()
    val iotDeviceRepository = new IotDeviceRepositoryInMemoryFuture()

    val userService = new UserService[Future](userRepository)
    val iotDeviceService = new IotDeviceService[Future](iotDeviceRepository, userRepository)

    val userName1 = "Ash Ketchum"
    val userName2 = "Brock"
    val userName3 = "Misty"

    await(userService.registerUser(userName1))
    await(userService.registerUser(userName2))
    await(userService.registerUser(userName3))


    val user1 = await(userService.getByUsername(userName1))
    val user2 = await(userService.getByUsername(userName2))
    val user3 = await(userService.getByUsername(userName3))

    assert(user1.get.id == 1)
    assert(user2.get.id == 2)
    assert(user3.get.id == 3)

    assert(user1.get.username == userName1)
    assert(user2.get.username == userName2)
    assert(user3.get.username == userName3)


    val deviceData1 = (user1.get.id, "Pikachu")
    val deviceData2 = (user2.get.id, "Onix")
    val deviceData3 = (user3.get.id, "Hypno's Naptime")

    (iotDeviceService.registerDevice _).tupled(deviceData1)
    (iotDeviceService.registerDevice _).tupled(deviceData2)
    (iotDeviceService.registerDevice _).tupled(deviceData3)

    assert(await(iotDeviceService.getByUser(user1.get.id)).headOption.get.id == 1)
    assert(await(iotDeviceService.getByUser(user2.get.id)).headOption.get.id == 2)
    assert(await(iotDeviceService.getByUser(user3.get.id)).headOption.get.id == 3)

    assert(await(iotDeviceService.getByUser(user1.get.id)).headOption.get.sn == deviceData1._2)
    assert(await(iotDeviceService.getByUser(user2.get.id)).headOption.get.sn != deviceData1._2)
    assert(await(iotDeviceService.getByUser(user3.get.id)).headOption.get.sn != deviceData1._2)
    assert(await(iotDeviceService.getByUser(user1.get.id)).headOption.get.sn != deviceData2._2)
    assert(await(iotDeviceService.getByUser(user2.get.id)).headOption.get.sn == deviceData2._2)
    assert(await(iotDeviceService.getByUser(user3.get.id)).headOption.get.sn != deviceData2._2)
    assert(await(iotDeviceService.getByUser(user1.get.id)).headOption.get.sn != deviceData3._2)
    assert(await(iotDeviceService.getByUser(user2.get.id)).headOption.get.sn != deviceData3._2)
    assert(await(iotDeviceService.getByUser(user3.get.id)).headOption.get.sn == deviceData3._2)

    assert(await(iotDeviceService.getByUser(user1.get.id)).headOption.get.userId == user1.get.id)
    assert(await(iotDeviceService.getByUser(user1.get.id)).headOption.get.userId != user2.get.id)
    assert(await(iotDeviceService.getByUser(user2.get.id)).headOption.get.userId == user2.get.id)
    assert(await(iotDeviceService.getByUser(user2.get.id)).headOption.get.userId != user3.get.id)
    assert(await(iotDeviceService.getByUser(user3.get.id)).headOption.get.userId == user3.get.id)
    assert(await(iotDeviceService.getByUser(user3.get.id)).headOption.get.userId != user1.get.id)

  }
}

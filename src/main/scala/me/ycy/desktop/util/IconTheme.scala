package me.ycy.desktop.util

import org.ini4j.Ini
import java.io.File

object IconTheme {
  case class Index(
    name: String,
    comment: String,
    inherits: List[String], // absent -> List()
    directories: List[Subdir],
    hidden: Boolean, // absent -> True
    example: Option[String]
  )

  sealed trait IconType
  object IconType {
    case object Fixed extends IconType
    case class Scalable(minSize: Int, maxSize: Int) extends IconType
    case class Threshold(threshold: Int) extends IconType
  }

  case class Subdir(
    name: String,
    size: Int,
    context: Option[String],
    `type`: IconType
  )

  def apply(name: String, base: List[String]) =
    new IconTheme(name, base)

  def apply(name: String, func: (List[String] ⇒ List[String])): IconTheme = {
    var base = List("/usr/share/pixmaps")

    val home = System.getenv("HOME")
    val xdg = System.getenv("XDG_DATA_DIRS")

    if (xdg != null) {
      base = xdg.split(":").toList.map(_ + "/icons") ++ base
    }

    if (home != null) {
      base ::= System.getenv("HOME") + "/.icons"
    }

    base = func(base)

    apply(name, base)
  }

  def apply(name: String): IconTheme = {
    apply(name, x ⇒ x)
  }

  val HiColor = IconTheme("hicolor")

  def findIconHelper(
    icon: String, size: Int, theme: IconTheme
  ): Option[String] = {
    val i0 = lookupIcon(icon, size, theme)
    if (i0.isDefined) return i0

    for (p ← theme.getIndex().inherits) {
      val t = IconTheme(p)
      val i1 = findIconHelper(icon, size, t)
      if (i1.isDefined) return i1;
    }

    None
  }

  def lookupIcon(
    iconName: String, size: Int, theme: IconTheme
  ): Option[String] = {
    for (subdir ← theme.getIndex().directories) {
      for (dir ← theme.base) {
        for (ext ← List("png", "svg", "xpm")) {
          if (directoryMatchesSize(subdir, size)) {
            val filename = dir + "/" + theme.name + "/" +
                subdir.name + "/" + iconName + "." + ext
            if (new File(filename).exists()) return Some(filename)
          }
        }
      }
    }

    var minimalSize = Integer.MAX_VALUE
    var closestFilename: Option[String] = None

    for (subdir ← theme.getIndex().directories) {
      for (dir ← theme.base) {
        for (ext ← List("png", "svg", "xpm")) {
          val filename = dir + "/" + theme.name + "/" +
          subdir.name + "/" + iconName + "." + ext
          if (new File(filename).exists() &&
            (directorySizeDistance(subdir, size) < minimalSize)) {
	        val closestFilename = Some(filename)
	        minimalSize = directorySizeDistance(subdir, size)
          }
        }
      }
    }

    closestFilename
  }

  def lookupFallbackIcon(
    iconName: String, base: List[String]
  ): Option[String] = {
    for (dir ← base) {
      for (ext ← List("png", "svg", "xpm")) {
        val filename = dir + "/" + iconName + "." + ext
        if (new File(filename).exists()) return Some(filename)
      }
    }

    None
  }

  def directoryMatchesSize(subdir: Subdir, iconsize: Int): Boolean = {
    val size = subdir.size
    subdir.`type` match {
      case IconType.Fixed ⇒
        size == iconsize
      case IconType.Scalable(min, max) ⇒
        (min <= iconsize) && (iconsize <= max)
      case IconType.Threshold(thres) ⇒
        (size - thres <= iconsize) && (iconsize <= size + thres)
    }
  }

  def directorySizeDistance(subdir: Subdir, iconsize: Int): Int = {
    val size = subdir.size

    subdir.`type` match {
      case IconType.Fixed ⇒
        (size - iconsize).abs
      case IconType.Scalable(min, max) ⇒
        if (iconsize < min)
          min - iconsize
        else if (iconsize > max)
          iconsize - max
        else
          0
      case IconType.Threshold(thres) ⇒ // NOTE, here different from reference
        if (iconsize < size - thres)
          size - thres - iconsize
        else if (iconsize > size + thres)
          iconsize - size - thres
        else
          0
    }
  }
}

class IconTheme(val name: String, val base: List[String]) {
  import IconTheme._

  val ini = new Ini()
  private[this] var index: Index = Index(
    name = name,
    comment = "",
    inherits = List(),
    directories = List(),
    hidden = true,
    example = None
  )


  // do refresh
  refresh()

  def getIndex() = index

  private[this] def extractType(subdir: String, size: Int): IconType = {
    ini.get(subdir, "Type") match {
      case "Fixed" ⇒ IconType.Fixed
      case "Scalable" ⇒
        val minSize =
          try {
            Integer.parseInt(ini.get(subdir, "MinSize"))
          }
          catch {
            case _: Exception ⇒ size
          }
        val maxSize =
          try {
            Integer.parseInt(ini.get(subdir, "MaxSize"))
          }
          catch {
            case _: Exception ⇒ size
          }
        IconType.Scalable(minSize, maxSize)
      case _ ⇒ // default is Threshold
        val thres =
          try {
            Integer.parseInt(ini.get(subdir, "Threshold"))
          }
          catch {
            case _: Exception ⇒ 2
          }
        IconType.Threshold(thres)
    }
  }

  def refresh() = {
    // find first index.theme
    for (i ← base) {
      val idx = new File(i + "/" + name + "/index.theme")
      try {
        ini.load(idx)
        val sect = ini.get("Icon Theme")
        assert(sect != null)
        val name = sect.get("Name")
        assert(name != null)
        val comment = sect.get("Comment")
        assert(comment != null)

        val inherits = sect.get("Inherits") match {
          case null ⇒
            List()
          case x ⇒
            x.split(",").toList
        }

        val directoriesRaw = sect.get("Directories")
        assert(directoriesRaw != null)

        val directories = directoriesRaw.split(",").toList.map {d ⇒
          try {
            val size = Integer.parseInt(ini.get(d, "Size"))
            val contextRaw = ini.get(d, "Context")
            val context =
              if (contextRaw == null)
                None
              else
                Some(contextRaw)
            val `type` = extractType(d, size)
            Some(Subdir(
              name = d,
              size = size,
              context = context,
              `type` = `type`
            ))
          }
          catch {
            case _: Exception => None
          }
        }.filter(_.isDefined).map(_.get)

        val hidden =
          sect.get("Hidden") match {
            case "true" ⇒ true
            case _ ⇒ false
          }

        val example = sect.get("Example") match {
          case null ⇒ None
          case x ⇒ Some(x)
        }

        index = Index(
          name = name,
          comment = comment,
          inherits = inherits,
          directories = directories,
          hidden = hidden,
          example = example
        )
      }
      catch {
        case _: Exception ⇒
      }
    }
  }

  def findIcon(icon: String, size: Int): Option[String] = {
    findIconHelper(icon, size, this) match {
      case Some(x) ⇒ Some(x)
      case None ⇒
        findIconHelper(icon, size, HiColor) match {
          case Some(x) ⇒ Some(x)
          case None ⇒ lookupFallbackIcon(icon, base)
        }
    }
  }
}

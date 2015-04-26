package bacala.util

/** Makes color text in console easier
  */
object ConsoleHelper {
  implicit class ColorText(val str: Any) extends AnyVal {
    import Console._

    // ANSI color foreground
    def black     = s"$BLACK$str$RESET"
    def red       = s"$RED$str$RESET"
    def green     = s"$GREEN$str$RESET"
    def yellow    = s"$YELLOW$str$RESET"
    def blue      = s"$BLUE$str$RESET"
    def magenta   = s"$MAGENTA$str$RESET"
    def cyan      = s"$CYAN$str$RESET"
    def white     = s"$WHITE$str$RESET"

    // ANSI color background
    def black_b   = s"$BLACK_B$str$RESET"
    def red_b     = s"$RED_B$str$RESET"
    def green_b   = s"$GREEN_B$str$RESET"
    def yellow_b  = s"$YELLOW_B$str$RESET"
    def blue_b    = s"$BLUE_B$str$RESET"
    def magenta_b = s"$MAGENTA_B$str$RESET"
    def cyan_b    = s"$CYAN_B$str$RESET"
    def white_b   = s"$WHITE_B$str$RESET"

    // ANSI styles
    def reset       = s"$RESET$str"
    def bold        = s"$BOLD$str$RESET"
    def underlined  = s"$UNDERLINED$str$RESET"
    def blink       = s"$BLINK$str$RESET"
    def reversed    = s"$REVERSED$str$RESET"
    def invisible   = s"$INVISIBLE$str$RESET"
  }
}

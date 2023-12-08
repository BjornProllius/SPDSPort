package wpds.interfaces

trait Location {
    def accepts(other: Location): Boolean
}
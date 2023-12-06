package wpds.wildcard

trait ExclusionWildcard[Location] extends Wildcard {
    def excludes(): Location
}
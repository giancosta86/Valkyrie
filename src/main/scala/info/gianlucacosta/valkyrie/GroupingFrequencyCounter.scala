/*^
  ===========================================================================
  Valkyrie
  ===========================================================================
  Copyright (C) 2017 Gianluca Costa
  ===========================================================================
  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
  ===========================================================================
*/

package info.gianlucacosta.valkyrie

private case class GroupingFrequencyCounter[T](items: Iterable[T]) {
  private val universeCount =
    items.size

  val frequencies: List[FrequencyTracker[T]] =
    items
      .groupBy(item => item)
      .map {
        case (item, occurrences) =>
          FrequencyTracker(
            item,
            occurrences.size,
            universeCount
          )
      }
      .toList
      .sortBy(_.frequency)
}

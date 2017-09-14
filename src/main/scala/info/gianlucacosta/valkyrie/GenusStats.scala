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

import info.gianlucacosta.balmung.lexicon.{Genus, Noun}

private case class GenusStats(
                               nouns: Iterable[Noun]
                             ) {
  require(nouns.nonEmpty)


  val (masculineCount, neuterCount, feminineCount): (Long, Long, Long) = {
    val genusMap =
      nouns
        .filter(_.getGenus.isPresent)
        .groupBy(_.getGenus.get())

    (
      genusMap.getOrElse(Genus.MASKULIN, List()).size,
      genusMap.getOrElse(Genus.NEUTRAL, List()).size,
      genusMap.getOrElse(Genus.FEMININ, List()).size
    )
  }

  val totalCount: Long = masculineCount + neuterCount + feminineCount


  def masculinePercentage: Double =
    masculineCount * 100.0 / totalCount


  def neuterPercentage: Double =
    neuterCount * 100.0 / totalCount


  def femininePercentage: Double =
    feminineCount * 100.0 / totalCount


  val mainGenus: Genus =
    if (masculinePercentage > neuterPercentage) {
      if (masculinePercentage > femininePercentage)
        Genus.MASKULIN
      else
        Genus.FEMININ
    } else {
      if (neuterPercentage > femininePercentage)
        Genus.NEUTRAL
      else
        Genus.FEMININ
    }


  val highestPercentage: Double =
    mainGenus match {
      case Genus.MASKULIN =>
        masculinePercentage

      case Genus.NEUTRAL =>
        neuterPercentage

      case Genus.FEMININ =>
        femininePercentage
    }


  val relevance: Long =
    (1e3 * highestPercentage + totalCount).toLong


  override def toString: String =
    f"(MASCULINE: ${masculinePercentage}%.2f%%, N: ${neuterPercentage}%.2f%%, F: ${femininePercentage}%.2f%%; G: ${mainGenus.getShortName}; T: ${totalCount}%d; R: ${relevance}%d)"

}
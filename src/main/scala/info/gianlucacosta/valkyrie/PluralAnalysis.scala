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

import java.io.PrintWriter
import java.nio.file.{Files, Path}

import info.gianlucacosta.balmung.lexicon.{Genus, Kasus, Noun, Numerus}
import info.gianlucacosta.balmung.transform.extensive.SuffixTransform

private object PluralAnalysis {
  def perform(outputDirPath: Path, nouns: Iterable[Noun]): Unit = {
    val pluralDirPath =
      outputDirPath.resolve("plural")

    Files.createDirectories(pluralDirPath)

    val prefilteredNouns =
      nouns
        .filter(Utils.hasSingleWordSingularNominative)

    computePluralsByEndingAndGenus(
      pluralDirPath,
      prefilteredNouns
    )
  }


  private def computePluralsByEndingAndGenus(
                                              pluralDirPath: Path,
                                              nouns: Iterable[Noun],
                                              maxEndingLength: Int = 7,
                                              minNounsCountPerRule: Int = 50
                                            ): Unit = {

    println("== COMPUTING PLURAL RULES BY ENDING AND GENUS ==")
    println()

    val outputFilePath = pluralDirPath.resolve("byEndingAndGenus.yml")

    val nounsWithBothNominativesAndWithGenus =
      nouns
        .filter(noun => {
          val singularNominativeOption =
            noun.getDeclension.getExpression(Numerus.SINGULAR, Kasus.NOMINATIV)

          val pluralNominativeOption =
            noun.getDeclension.getExpression(Numerus.PLURAL, Kasus.NOMINATIV)

          singularNominativeOption.isPresent &&
            pluralNominativeOption.isPresent &&
            singularNominativeOption.get().length <= pluralNominativeOption.get().length

        })
        .filter(_.getGenus.isPresent)


    val rules =
      Range.inclusive(1, maxEndingLength)
        .flatMap(endingLength => {
          nounsWithBothNominativesAndWithGenus
            .filter(noun => Utils.hasLongEnoughSingularNominative(noun, endingLength))
            .groupBy(noun => {
              val ending =
                Utils.getEnding(
                  endingLength,

                  noun
                    .getDeclension
                    .getExpression(Numerus.SINGULAR, Kasus.NOMINATIV)
                    .get()
                )

              (
                ending,
                noun.getGenus.get()
              )
            })
            .filter {
              case ((ending, genus), groupedNouns) =>
                groupedNouns.size >= minNounsCountPerRule
            }
            .filter {
              case ((ending, genus), groupedNouns) =>
                Character.isLowerCase(ending(0))
            }
            .map {
              case ((ending, genus), groupedNouns) =>
                (ending, genus) ->
                  groupedNouns.map(noun => {
                    val singularNominativ =
                      noun.getDeclension.getExpression(Numerus.SINGULAR, Kasus.NOMINATIV).get()

                    val pluralNominativ =
                      noun.getDeclension.getExpression(Numerus.PLURAL, Kasus.NOMINATIV).get()

                    SuffixTransform.compute(singularNominativ, pluralNominativ)
                  })
                    .filter(_.getSuffixToRemove.length <= maxEndingLength)
            }

            .map {
              case ((ending, genus), transforms) =>
                (ending, genus) ->
                  GroupingFrequencyCounter[String](
                    transforms.map(_.toString)
                  )
                    .frequencies
                    .reverse
            }
        })


    println(s"Rules ready: ${rules.size}")

    val compactRules =
      rules
        .filter {
          case ((ending, genus), transformFrequencies) =>
            !rules
              .filter {
                case ((rawEnding, _), _) =>
                  rawEnding.length < ending.length
              }
              .exists {
                case ((rawEnding, rawGenus), rawTransformFrequencies) =>
                  ending.endsWith(rawEnding) &&
                    (genus == rawGenus) &&
                    (transformFrequencies.map(_.item).toSet == rawTransformFrequencies.map(_.item).toSet)
              }
        }
        .toMap
        .groupBy {
          case ((ending, genus), transformFrequencies) =>
            transformFrequencies.size
        }


    println(s"Compact rules ready: ${compactRules.values.map(_.size).sum}")

    println()

    writePluralByEndingAndGenus(
      outputFilePath,
      compactRules
    )

    println()
    println()
    println()
  }


  private def writePluralByEndingAndGenus(
                                           outputFilePath: Path,
                                           rulesMap: Map[Int, Map[(String, Genus), List[FrequencyTracker[String]]]],
                                           minFrequencyPercentage: Double = 1
                                         ): Unit = {
    val outputWriter = new PrintWriter(Files.newBufferedWriter(outputFilePath))

    try {
      val sortedRules =
        rulesMap
          .map {
            case (transformsCount, pluralRules) =>
              transformsCount ->
                pluralRules
                  .toList
                  .map {
                    case ((ending, genus), transformFrequencies) =>
                      (ending, genus) ->
                        transformFrequencies
                          .filter(
                            _.percentage >= minFrequencyPercentage
                          )
                  }
                  .filter {
                    case ((ending, genus), transformFrequencies) =>
                      transformFrequencies.nonEmpty
                  }
                  .sortBy {
                    case ((ending, genus), transformFrequencies) =>
                      (
                        100 - transformFrequencies.head.percentage,
                        ending,
                        genus
                      )
                  }
          }
          .filter {
            case (transformsCount, pluralRules) =>
              pluralRules.nonEmpty
          }
          .toList
          .sortBy {
            case (transformsCount, pluralRules) =>
              transformsCount
          }


      sortedRules
        .foreach {
          case (transformsCount, pluralRules) =>
            outputWriter.println(s"${transformsCount}:")

            pluralRules
              .foreach {
                case ((ending, genus), transformFrequencies) =>
                  outputWriter.println(" - ")
                  outputWriter.println(s"   ending: ${ending}")
                  outputWriter.println(s"   genus: ${genus}")
                  outputWriter.println(s"   totalCount: ${transformFrequencies.head.universeCount}")
                  outputWriter.println(s"   maxPercentage: ${transformFrequencies.head.percentage}")
                  outputWriter.println(s"   transformFrequencies:")

                  transformFrequencies
                    .foreach(transformFrequency => {
                      outputWriter.println(s"    - ")

                      outputWriter.println(s"      transform: '${transformFrequency.item}'")
                      outputWriter.println(s"      count: ${transformFrequency.frequency}")
                      outputWriter.println(f"      percentage: ${transformFrequency.percentage}%.2f")
                    })
              }
        }
    } finally {
      outputWriter.close()
    }
  }
}

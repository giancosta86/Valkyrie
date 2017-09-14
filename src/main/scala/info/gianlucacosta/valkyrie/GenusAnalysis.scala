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

import info.gianlucacosta.balmung.lexicon.{Kasus, Noun, Numerus}

private object GenusAnalysis {
  def perform(outputDirPath: Path, nouns: Iterable[Noun]): Unit = {
    val genusDirPath =
      outputDirPath.resolve("genus")

    Files.createDirectories(genusDirPath)

    val prefilteredNouns =
      nouns
        .filter(Utils.hasSingleWordSingularNominative)

    computeGenusStatsByEnding(
      genusDirPath,
      prefilteredNouns
    )


    computeGenusStatsByBeginning(
      genusDirPath,
      prefilteredNouns
    )
  }


  private def computeGenusStatsByEnding(genusDirPath: Path, nouns: Iterable[Noun]): Unit = {
    val genusStatsByEnding =
      computeGenusStatsByGrouping(
        nouns,
        (endingLength, singularNominative) => Utils.getEnding(endingLength, singularNominative)
      )

    println("== COMPUTING GENUS STATS BY ENDING ==")
    println()

    writeGenusStatsByGrouping(
      genusDirPath.resolve("byEnding.yml"),
      genusStatsByEnding,
      ending => "-" + ending
    )

    val filteredGenusStatsByEnding =
      compactGenusStats(
        genusStatsByEnding,
        (potentiallyImplying, potentiallyImplied) => potentiallyImplied.endsWith(potentiallyImplying),
        1.0
      )

    writeGenusStatsByGroupingAndRelevance(
      genusDirPath.resolve("byEndingAndRelevance.yml"),
      filteredGenusStatsByEnding,
      ending => "-" + ending
    )

    println()
    println()
    println()
  }


  private def computeGenusStatsByGrouping(
                                           nouns: Iterable[Noun],
                                           groupingFunction: (Int, String) => String,
                                           maxGroupingLength: Int = 6,
                                           minGroupedNounsCount: Long = 100
                                         ): Map[Int, Map[String, GenusStats]] = {
    Range
      .inclusive(1, maxGroupingLength)
      .map(groupingLength =>
        groupingLength ->
          nouns
            .filter(noun => Utils.hasLongEnoughSingularNominative(noun, groupingLength))
            .groupBy(noun => {
              groupingFunction(
                groupingLength,

                noun
                  .getDeclension
                  .getExpression(Numerus.SINGULAR, Kasus.NOMINATIV)
                  .get()
              )
            })
            .filter {
              case (grouping, groupedNouns) =>
                groupedNouns.size >= minGroupedNounsCount
            }
            .map {
              case (grouping, groupedNouns) =>
                grouping -> GenusStats(
                  groupedNouns
                )
            }
      )
      .toMap
  }


  private def compactGenusStats(
                                 statsMap: Map[Int, Map[String, GenusStats]],
                                 implicationOnGroupingStrings: (String, String) => Boolean,
                                 maxHighestPercentageDeltaForImplication: Double
                               ): Map[Int, Map[String, GenusStats]] = {
    statsMap
      .map {
        case (groupingStringLength, groupingStatsMap) =>
          val flatStatsList: Iterable[(String, GenusStats)] =
            statsMap
              .filter {
                case (flatGroupingStringLength, _) =>
                  flatGroupingStringLength < groupingStringLength
              }
              .values
              .flatten

          val filteredGroupingStatsMap =
            groupingStatsMap.filter {
              case (groupingString, groupingStats) =>
                val isGroupingImplied =
                  flatStatsList.exists {
                    case (flatGroupingString, flatStats) =>
                      implicationOnGroupingStrings(flatGroupingString, groupingString) &&
                        (flatStats.totalCount >= groupingStats.totalCount) &&
                        (flatStats.highestPercentage + maxHighestPercentageDeltaForImplication >= groupingStats.highestPercentage)
                  }

                !isGroupingImplied
            }

          groupingStringLength -> filteredGroupingStatsMap
      }
      .filter {
        case (groupingStringLength, groupingStatsMap) =>
          groupingStatsMap.nonEmpty
      }
  }


  private def writeGenusStatsByGrouping(
                                         outputFilePath: Path,
                                         statsMap: Map[Int, Map[String, GenusStats]],
                                         groupingStringProcessor: String => String
                                       ): Unit = {
    val outputWriter = new PrintWriter(Files.newBufferedWriter(outputFilePath))

    try {
      statsMap
        .keys
        .toList
        .sorted
        .foreach(groupingLength => {
          println(s"=== GROUPING NOUNS WITH GROUPING STRINGS OF ${groupingLength} LETTER(S) ===")
          println()

          outputWriter.println(s"${groupingLength}:")


          val groupingStats =
            statsMap(groupingLength)


          groupingStats
            .toList
            .sortBy(_._1)
            .foreach {
              case (groupingString, stats) =>
                writeGenusStats(
                  outputWriter,
                  groupingStringProcessor(groupingString),
                  stats,
                  1
                )
            }

          println()
          println()
        })
    } finally {
      outputWriter.close()
    }
  }


  private def writeGenusStats(
                               outputWriter: PrintWriter,
                               groupingString: String,
                               stats: GenusStats,
                               leadingSpacesCount: Int
                             ): Unit = {
    println(s"${groupingString} --> ${stats}")

    val leadingSpaces =
      " " * leadingSpacesCount

    outputWriter.println(s"${leadingSpaces}${groupingString}:")
    outputWriter.println(f"${leadingSpaces} masculinePercentage: ${stats.masculinePercentage}%.2f")
    outputWriter.println(f"${leadingSpaces} neuterPercentage: ${stats.neuterPercentage}%.2f")
    outputWriter.println(f"${leadingSpaces} femininePercentage: ${stats.femininePercentage}%.2f")
    outputWriter.println(s"${leadingSpaces} totalCount: ${stats.totalCount}")
    outputWriter.println(s"${leadingSpaces} relevance: ${stats.relevance}")
    outputWriter.println(s"${leadingSpaces} mainGenus: ${stats.mainGenus}")
    outputWriter.println()
  }


  private def writeGenusStatsByGroupingAndRelevance(
                                                     outputFilePath: Path,
                                                     statsMap: Map[Int, Map[String, GenusStats]],
                                                     groupingStringProcessor: String => String
                                                   ): Unit = {
    val outputWriter = new PrintWriter(Files.newBufferedWriter(outputFilePath))

    try {
      println(s"=== SORTING STATS BY RELEVANCE ===")
      println()

      val statsSortedByRelevance =
        statsMap
          .values
          .flatten
          .toList
          .sortBy {
            case (groupingString, stats) =>
              stats.relevance
          }
          .reverse

      statsSortedByRelevance
        .foreach {
          case (groupingString, stats) =>
            writeGenusStats(
              outputWriter,
              groupingStringProcessor(groupingString),
              stats,
              0
            )
        }

      println()
      println()

    } finally {
      outputWriter.close()
    }
  }


  private def computeGenusStatsByBeginning(genusDirPath: Path, nouns: Iterable[Noun]): Unit = {
    val genusStatsByBeginning =
      computeGenusStatsByGrouping(
        nouns,
        (beginningLength, nominative) => nominative.substring(0, beginningLength)
      )

    println("== COMPUTING GENUS STATS BY BEGINNING ==")
    println()

    writeGenusStatsByGrouping(
      genusDirPath.resolve("byBeginning.yml"),
      genusStatsByBeginning,
      beginning => beginning + "-"
    )

    val filteredGenusStatsByBeginning =
      compactGenusStats(
        genusStatsByBeginning,
        (potentiallyImplying, potentiallyImplied) => potentiallyImplied.startsWith(potentiallyImplying),
        1.0
      )

    writeGenusStatsByGroupingAndRelevance(
      genusDirPath.resolve("byBeginningAndRelevance.yml"),
      filteredGenusStatsByBeginning,
      beginning => beginning + "-"
    )

    println()
    println()
    println()
  }
}

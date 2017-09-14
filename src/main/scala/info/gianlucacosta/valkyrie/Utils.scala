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

import info.gianlucacosta.balmung.lexicon.{Kasus, Noun, Numerus}

private object Utils {
  def getEnding(endingLength: Int, word: String): String = {
    word.substring(
      word.length - endingLength
    )
  }

  def hasLongEnoughSingularNominative(noun: Noun, minimumLength: Int): Boolean = {
    val singularNominativeOption =
      noun.getDeclension
        .getExpression(Numerus.SINGULAR, Kasus.NOMINATIV)

    singularNominativeOption.isPresent &&
      singularNominativeOption.get().length >= minimumLength
  }

  def hasSingleWordSingularNominative(noun: Noun): Boolean = {
    val singularNominative =
      noun.getDeclension.getExpression(Numerus.SINGULAR, Kasus.NOMINATIV)

    singularNominative.isPresent && singularNominative.get().matches("^[A-Za-zßÄäÖöÜü]+$")
  }
}

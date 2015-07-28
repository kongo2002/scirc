
# scirc

*scirc* is a basic [IRC][irc] server written in [Scala][scala] using the
[akka][akka] framework.

> CAUTION: the project is a work-in-progress and not even to be considered
> "alpha".


## Installation

The setup is pretty straightforward for a [SBT][sbt] based package:

    $ git clone git://github.com/kongo2002/scirc.git
    $ cd scirc
    $ sbt compile


## Usage

The usage is a simple invocation of `sbt run` that will start a *scirc* instance
on the default [IRC][irc] port `6667` at `localhost`.

    $ sbt run


## Tests

You may run the test suite with `sbt test` if you like.


## Maintainer

*scirc* is written by Gregor Uhlenheuer. You can reach me at
<kongo2002@gmail.com>.


## License

*scirc* is licensed under the [Apache license][apache], Version 2.0

> Unless required by applicable law or agreed to in writing, software
> distributed under the License is distributed on an "AS IS" BASIS,
> WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
> See the License for the specific language governing permissions and
> limitations under the License.


[apache]: http://www.apache.org/licenses/LICENSE-2.0
[irc]: https://en.wikipedia.org/wiki/Internet_Relay_Chat
[scala]: http://www.scala-lang.org/
[akka]: http://akka.io/
[sbt]: http://www.scala-sbt.org/

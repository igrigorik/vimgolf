# Load the Rails application.
require_relative 'application'
require 'keylog'

include TweetButton

MAX_FILESIZE = 1024*10

ADMINS = [
  'igrigorik',
  'wolever',
  'singpolyma',
  'opello',
  'josh_triplett',
  'ujjwol',
  'gumnos',
  'federicogalassi',
  'timvisher',
  'udioica',
  'braxler',
  'sakigw'
]

# Initialize the Rails application.
Rails.application.initialize!

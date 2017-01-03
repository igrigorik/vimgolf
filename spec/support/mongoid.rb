module Mongoid
  module Matchers
    class HaveFieldMatcher
      def initialize(*fields)
        @fields = fields
      end

      def matches?(klass)
        @klass = klass
        @errors = []

        @errors = @fields.each_with_object([]) do |field, errors|
          if !@klass.fields.keys.include?(field.to_s)
            errors << "no field named #{field}"
          end
        end

        @errors.empty?
      end

      def description
        field_pluralized = @fields.length > 1 ? "fields" : "field"
        "have #{field_pluralized} named #{@fields.to_sentence}"
      end

      def failure_message
        "Expected #{@klass.class} to #{description}, got #{@errors.to_sentence}"
      end
    end

    def have_fields(*args)
      HaveFieldMatcher.new(*args)
    end
  end
end

RSpec.configure do |configuration|
  configuration.include Mongoid::Matchers

  # rspec-rails 3 will no longer automatically infer an example group's spec type
  # from the file location. You can explicitly opt-in to the feature using this
  # config option.
  # To explicitly tag specs without using automatic inference, set the `:type`
  # metadata manually:
  #
  #     describe ThingsController, :type => :controller do
  #       # Equivalent to being in spec/controllers
  #     end
  configuration.infer_spec_type_from_file_location!
end
